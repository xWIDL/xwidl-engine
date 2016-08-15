module Session (run) where

import Language.Dafny.Translate
import Language.Dafny.AST
import Language.Dafny.Request
import Language.Dafny.Analyze

import Language.XWIDL.Spec
import Language.XWIDL.WebIDL
import Language.WebIDL.Parser

import Language.JS.Type
import Language.JS.Platform
import Control.JS.Utils.Server

import qualified Language.WebIDL.AST as W

import Model
import State
import Util
import Type

import Control.Monad.State hiding (join)
import Control.Monad (forM_)
import Control.Monad.Trans.Except

import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Char (toLower)
import Data.String.Utils
import Data.Atomics.Counter
import Data.Aeson
import Data.Either (partitionEithers)
import Text.Regex

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import System.IO
import Text.PrettyPrint.Leijen (pretty, Pretty)

run :: IO ()
run = do
    counter <- newCounter 0
    logging "Engine launched, waiting for connection"
    startServe $ \handler -> do
        hSetBuffering stdout NoBuffering
        hSetBuffering handler NoBuffering

        snum <- incrCounter 1 counter
        logging $ "Creating session #" ++ show snum
        
        Just (CBoot (Domains m) idl) <- eGetLine handler
        logging $ "Get domains: " ++ show m

        prelude <- readFile "prelude.dfy"
        logging "Prelude Dafny loaded."

        case parseIDL idl of
            Left e -> warning $ "Parse of IDL failde: " ++ show e
            Right idlAST -> do
                logging "IDL is parsed."
                case transDefsToSpec idlAST of
                    Left e -> warning $ "Translation of IDL failde: " ++ show e
                    Right spec -> do
                        logging $ "Spec is ready: " ++ show spec
                        case translateSpec spec of
                            Left e -> warning $ "Translation of spec failed: " ++ e
                            Right (traits, datatypes) -> do
                                putStrLn "// ------ traits ------- "
                                mapM_ (print . pretty) traits

                                evalStateT loop (SessionState {
                                    _heap = initHeap,
                                    _tlm  = initTargetMethod,
                                    _spec = spec,
                                    _handler = handler,
                                    _snum = snum,
                                    _traits = traits,
                                    _traitsNew = Nothing,
                                    _datatypes = datatypes,
                                    _pDomains = M.fromList m,
                                    _prelude = prelude,
                                    _namer = initNamer
                                })
                                logging $ "Ending session #" ++ show snum
    where
        initTargetMethod = TopLevelMethod {
            _tlName = "Main",
            _tlArgs = M.singleton "cb" (DTyClass "CallbackTrait"),
            _tlRequires = [DTerm (DRel NotEqual (DVal (DVar "cb")) (DVal (DPrim PNull)))],
            _tlBody = []
        }

loop :: Session ()
loop = do
    logging "loop..."
    handler <- _handler <$> get
    mCmd <- liftIO $ eGetLine handler
    case mCmd of
        Just cmd -> do
            logging $ "Request " ++ show cmd
            eErr <- runExceptT (dispatch cmd)
            case eErr of
                Right continue | continue     -> loop
                               | not continue -> return ()
                Left errMsg -> reply (InvalidReqeust errMsg) >> loop
        Nothing -> do
            warning $ "Invalid request"
            reply (InvalidReqeust $ show mCmd)
            loop

-- Return True to hint continuation, return False to terminate session
dispatch :: Command -> ServeReq Bool
dispatch = \case
    CCall lvar f args -> do
        op <- lookupOperationWithLvar f lvar
        handleUnionCall lvar (Op op) args
        return True
    CGet lvar name -> handleGet lvar name >> return True
    CSet lvar name val -> handleSet lvar name val >> return True
    CNew name Nothing -> handleNewDef name >> return True
    CNew name (Just args) -> handleNewCons name args >> return True
    CEnd -> return False
    _ -> reply (InvalidReqeust "Invalid cmd") >> return True

handleSet :: LVar -> Name -> JsVal -> ServeReq ()
handleSet lvar name val = do
    x <- compileLVar lvar
    let aNameStr = unName name
    iname <- lvarToIfaceName lvar
    ix <- compileLVar (LInterface iname)
    eReport <- do
                    fname <- addValueMethod (unName iname) val
                    addStmt $ SVarAssign (x ++ "." ++ aNameStr) (DCall ix fname [])
                    getSat
    if reportToBool eReport
        then reply $ Sat (nullJsRetVal, Nothing)
        else reply $ Unsat "Can't set"

handleNewDef :: Name -> ServeReq ()
handleNewDef iname = do
    jsRetVal <- getJsValResult (DNew (unName iname) [])
                               (TyInterface iname)
                               (\_ -> return ())
                               (\_ -> error "Unreachable")
    reply $ Sat (jsRetVal, Nothing)

handleNewCons :: Name -> [JsUnionVal] -> ServeReq ()
handleNewCons iname uvals = do
    types <- mapM inferJsUnionValType uvals
    cons <- lookupCons iname types
    handleUnionCall (LInterface iname) (Cons iname cons) uvals

handleGet :: LVar -> Name -> ServeReq ()
handleGet lvar name@(Name nameStr) = do
    iname <- lvarToIfaceName lvar
    mConst <- lookupConsts iname name
    jsValRet <- case mConst of
                    Just prim -> return $ JVRPrimPrecise prim
                    Nothing -> do
                        ty <- lookupAttr iname name
                        x <- compileLVar lvar
                        getJsValResult (DTerm (DAccess x nameStr))
                                       ty
                                       (\_ -> return ())
                                       (inlineAssCtx (\_ -> return ()) (DTerm (DAccess x nameStr)))
    reply $ Sat (jsValRet, Nothing)

handleUnionCall :: LVar -> OperationOrConstructor -> [JsUnionVal] -> ServeReq ()
handleUnionCall lvar ooc uvals = do
    -- regenerate mono-calls
    let args = oocArgs ooc ++ oocOptArgs ooc
    if length uvals /= length args
        then reply $ InvalidReqeust "Wrong number of arguments"
        else do
            let pairs = zip uvals (map _argTy args)
            logging $ "handleUnionCall-pairs: " ++ show pairs
            singlified <- mapM singlify pairs
            logging $ "handleUnionCall-singlified: " ++ show singlified
            let calls = merge singlified

            logging $ "handleUnionCall-calls: " ++ show calls

            forM_ calls $ \(vals, argtys) -> do
                -- compile non-optional arguments
                handleSingleCall lvar ooc vals argtys

handleSingleCall :: LVar -> OperationOrConstructor -> [JsImmVal] -> [IType] -> ServeReq ()
handleSingleCall lvar ooc vals tys = do
    let nargs = length (oocArgs ooc) + length (oocOptArgs ooc)
    let nopts = length (oocOptArgs ooc)
    let argNames = (map _argName $ oocArgs ooc ++ oocOptArgs ooc)
    (args, cbs) <- partitionEithers . concat <$>
                       forM (zip3 vals tys argNames)
                            (\(val, ty, argn) -> do
                                let f = \case
                                            JVClos n -> do
                                                let cbspec = queryCallbackSpec ooc argn
                                                cb <- lookupCb ty
                                                return [Left  (DTerm (DVal (DVar "cb"))),
                                                        Right (n, cbspec, cb)]
                                            val -> (\v -> [Left v]) <$> compileNonCbJsVal val ty
                                case val of
                                    ImmVal val -> f val
                                    ImmApp fname val -> f val)
                                        -- (ls, rs) <- partitionEithers <$> f val
                                        -- return (map (\e -> Left (DApp (unName fname) [e])) ls ++ map Right rs))
    let nnonopt = nargs - nopts
    args'' <- mapM (\a -> exprToTerm a >>= \a' -> return (DTerm (DApp "Some" [a'])))
                   (drop nnonopt args)
    let args' = take nnonopt args ++
                args'' ++
                take (nargs - length args) (repeat $ DTerm (DVal (DVar "None")))

    -- comopile callback replies
    cbsret <- if cbs == [] then return Nothing else Just <$> compileCbs lvar ooc args' cbs

    -- compile value replies
    x <- compileLVar lvar

    let effM = tryEffect ooc args'

    case oocRet ooc of
        Nothing -> do -- invocation, no return value
            let Op op = ooc
            ret <- checkInvocation x (unName $ _imName op) args' (effM x "_")
            if ret
                then reply $ Sat (nullJsRetVal, cbsret)
                else reply $ Unsat "Invalid invocation"
        Just retty -> do -- evaluation, with return value
            tmArgs <- mapM exprToTerm args'
            let e = case ooc of
                        Op op -> DCall x (unName $ _imName op) tmArgs
                        Cons iname cons -> DNew (unName iname) tmArgs
            logging $ "handleSingleCall: " ++ show e ++ ", retty: " ++ show retty
            jsRetVal <- getJsValResult e
                                       retty
                                       (effM x)
                                       (inlineAssCtx (effM x) e)
            reply $ Sat (jsRetVal, cbsret)

tryEffect :: OperationOrConstructor -> [DyExpr] -> String -> String -> ServeReq ()
tryEffect ooc args xOld xNew = do
    let x = case ooc of
                Op _ -> xOld
                Cons _ _ -> xNew
    case oocMEffects ooc of
        Nothing -> return ()
        Just effStr -> do
            logging $ "tryEffect of " ++ show effStr
            let argNames = map _argName (oocArgs ooc ++ oocOptArgs ooc)
            let effStr' = subRegex (mkRegex "this\\.") effStr (x ++ ".")
            let effStr'' = foldr (\(name, arg) eStr -> subRegex (mkRegex ("\\$" ++ unName name))
                                                                eStr
                                                                ("(" ++ prettyShow arg ++ ")")) -- XXX: not a clean solution though
                                 effStr' (zip argNames args)
            addStmt (SStrBlock effStr')

checkInvocation :: String -> String -> [DyExpr] -> ServeReq () -> ServeReq Bool
checkInvocation x fname args effM = do
    tmArgs <- mapM exprToTerm args
    addStmt (SInvoke x fname tmArgs)
    effM
    reportToBool <$> getSat


-- If nothing, means failed
getJsValResult :: DyExpr -> IType -> (String -> ServeReq ()) -> AssContext -> ServeReq JsValResult
getJsValResult dyexpr ty effM ctx =
    case ty of
        TyInterface iname -> getObjResult iname
        TyNullable ty -> getJsValResult dyexpr ty effM ctx
        TyBuiltIn iname -> getObjResult iname
        TyAny -> getObjResult (Name "Any")
        TyObject -> getObjResult (Name "Object")
        TyBoolean -> getPrimResult PTyBool ctx
        TyInt -> getPrimResult PTyInt ctx
        TyFloat -> getPrimResult PTyNumber ctx
        TyDOMString -> getPrimResult PTyString ctx
        TyArray _ -> throwE $ "array is not supported yet"
    where
        getObjResult iname = do
            (r, vname) <- allocOnHeap iname
            addStmt (SVarDef vname dyexpr)
            effM vname
            return $ (JVRRef r)

getPrimResult :: PrimType -> AssContext -> ServeReq JsValResult
getPrimResult pty ctx = do
    domainMap <- _pDomains <$> get
    case M.lookup pty domainMap of
        Just domains -> do
            let assertions = domainsToAssertions domains
            (_, flags) <- head <$> flip filterM assertions (\(assert, _) -> reportToBool <$> ctx assert)
            return (JVRPrim pty flags)
        Nothing -> throwE $ "Invalid pty for domainMap key: " ++ show pty

compileCbs :: LVar -> OperationOrConstructor -> [DyExpr] -> [CallbackTriple] -> ServeReq JsCallbackResult
compileCbs lvar ooc noncbargs cbs = do
    logging $ "compileCbs-cbs: " ++ show cbs
    iname <- unName <$> lvarToIfaceName lvar
    let Op op = ooc -- XXX: need to support constructor?
    let fname = unName $ _imName op
    let checkSat = do
                        f <- fresh
                        x <- compileLVar lvar
                        lhs <- fresh
                        tempTLM $ do
                            tmArgs <- mapM exprToTerm noncbargs
                            addStmt (SVarDef lhs (DCall x fname tmArgs))
                            getSat

    mBranchs <- forM cbs $ \(n, CallbackSpec _ reqe withEs, cb) -> do

        eReport <- withModifiedMethod iname fname (andRequires reqe) checkSat

        if reportToBool eReport
            then do
                jsRetVals <- forM (zip withEs (_cArgs cb)) $ \(withE, arg) -> do
                    let ITySingle ty = _argTy arg
                    getJsValResult (DStrRepr withE) ty (\_ -> return ()) $ \(JAssert name je) -> do
                        je' <- compileExpr je
                        let x = DVal (DVar (unName name))
                        withModifiedMethod
                            iname fname
                            ( letBindRequires (unName name) (DStrRepr withE)
                            . andRequires (prettyShow je'))
                            checkSat
                return $ Just jsRetVals
            else return Nothing

    return $ JsCallbackResult mBranchs

compileNonCbJsVal :: JsVal -> IType -> ServeReq DyExpr
compileNonCbJsVal val = \case
    TyInterface i -> compileIfaceOrDictOrADT val i
    TyDOMString -> compilePrim val PTyString
    TyInt -> compilePrim val PTyInt
    TyFloat -> compilePrim val PTyNumber
    TyAny -> compileIfaceOrDictOrADT val (Name "Any")
    TyNullable ty -> compileNonCbJsVal val ty
    TyBoolean -> compilePrim val PTyBool
    TyObject -> compileIfaceOrDictOrADT val (Name "Object")
    TyBuiltIn i -> compileIfaceOrDictOrADT val i
    TyArray _ -> throwE "array type is not supported yet"

compileIfaceOrDictOrADT :: JsVal -> Name -> ServeReq DyExpr
compileIfaceOrDictOrADT jsval iname = do
    mDef <- lookupDefinition iname
    case mDef of
        Nothing -> do
            datatypes <- _datatypes <$> get
            case M.lookup (unName iname) datatypes of
                Just _ -> do
                    ty <- inferJsValType jsval
                    let consName = getADTConsName (unName iname) ty
                    innerE <- compileNonCbJsVal jsval ty
                    innerTm <- exprToTerm innerE
                    return (DTerm $ DApp consName [innerTm])
                Nothing -> 
                    throwE $ "Invalid definition name: " ++ show iname
        Just def ->
            case def of
                DefInterface iface -> compileIface jsval iface
                DefDictionary dict -> compileDict jsval dict
                DefCallback _ -> throwE $ "Delayed callback compilation: " ++ show iname
                DefException _ -> throwE $ "Invalid interface name: " ++ show iname

compileIface :: JsVal -> Interface -> ServeReq DyExpr
compileIface (JVRef jref) iface = do
     JsObj iname <- lookupObj jref
     if iname == _iName iface
        then (DTerm . DVal . DVar) <$> lookupBinding jref
        else throwE $ "Inconsistency between jref and iface type"
compileIface otherval _ = throwE $ "Invalid jsval for interface: " ++ show otherval

compileDict :: JsVal -> Dictionary -> ServeReq DyExpr
compileDict (JVDict m) dict = do
    let args = M.fromList $ map (\(DictionaryMember ty x _) -> (unName x, iTypeToDyType ty))
                                (_dmembers dict)

    dyexprs <- forM m $ \(name, jsval) -> do
                            case M.lookup (unName name) args of
                                Nothing -> throwE $ "No such member " ++ show name ++
                                                    " in dictionary " ++ show (_dname dict)
                                Just ty -> (name,) <$> compileNonCbJsVal jsval (dyTypeToIType ty)
    x <- lookupPlatObj (_dname dict)
    -- dyexprs
    dx <- fresh
    addStmt (SVarDef dx (DNew (unName $ _dname dict) []))
    forM_ dyexprs $ \(name, de) -> do
        addStmt (SVarAssign (dx ++ "." ++ unName name) de)
    return $ DTerm (DVal (DVar dx))

compileDict otherval _ = throwE $ "Invalid jsval for dictionary: " ++ show otherval

compilePrim :: JsVal -> PrimType -> ServeReq DyExpr
compilePrim (JVPrim pty assert) _ = do
    x <- fresh
    let je = app assert (Name x)
    de <- compileExpr je
    addArg x (pTyToDTy pty)
    addRequire de
    return (DTerm (DVal (DVar x)))

compileLVar :: LVar -> ServeReq String
compileLVar = \case
    LRef r -> lookupBinding r
    LInterface iname -> lookupPlatObj iname

compileExpr :: JsExpr -> ServeReq DyExpr
compileExpr (JEVar x) = return (DTerm (DVal $ DVar (unName x)))
compileExpr (JEPrim prim) = return (DTerm (DVal (DPrim prim)))
compileExpr (JEBinary op e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    tm1 <- exprToTerm e1'
    tm2 <- exprToTerm e2'
    return (DTerm (DRel op tm1 tm2))
compileExpr other = throwE $ "Can't compile Expr " ++ show other

-- Misc

andRequires :: String -> TraitMemberMethod -> TraitMemberMethod
andRequires r = modifyRequires (\case
                                    Just s  -> Just ("(" ++ s ++ ") && " ++ r)
                                    Nothing -> Just r)

letBindRequires :: String -> DyExpr -> TraitMemberMethod -> TraitMemberMethod
letBindRequires x e = modifyRequires (fmap (\s -> "var " ++ x ++ " := " ++ prettyShow e ++ "; (" ++ s ++ ")"))

modifyRequires :: (Maybe String -> Maybe String) -> TraitMemberMethod -> TraitMemberMethod
modifyRequires f m = m { _tmRequires = f (_tmRequires m) }

modifyEnsures :: (Maybe String -> Maybe String) -> TraitMemberMethod -> TraitMemberMethod
modifyEnsures f m = m { _tmEnsures = f (_tmEnsures m) }

queryCallbackSpec :: OperationOrConstructor -> Name -> CallbackSpec
queryCallbackSpec ooc x = 
    case ooc of
        Op op -> head $ filter (\(CallbackSpec x' _ _) -> x' == x) (_imCbs op)
        Cons _ _ -> error "can't query constructor for callback"

getSat :: ServeReq Report
getSat = do
    datatypes <- _datatypes <$> get
    tlm <- _tlm <$> get
    traits <- lookupTraits
    modify (\s -> s { _names = M.empty, _traitsNew = Nothing })
    prelude <- _prelude <$> get
    let src = prelude ++ "\n" ++ pprintDatatypes datatypes ++ "\n" ++
              unlines (map prettyShow $ M.elems traits) ++ "\n" ++ show (pretty tlm)
    logging ("Getting sat from REST...tlm: \n" ++ src)
    ans <- liftIO $ askDafny (Local "/home/zhangz/xwidl/dafny/Binaries") src
    case ans of
        Right ret -> do
            logging ("Got sat: " ++ show ret)
            return ret
        Left err -> error $ "Dafny connection error: " ++ err

reportToBool :: Report -> Bool
reportToBool = \case
    Verified -> True
    Failed _ -> False

domainsToAssertions :: [JAssert] -> [(JAssert, [Bool])]
domainsToAssertions domAsses =
    let (JAssert x _) = head domAsses
        es      = map (\(JAssert _ e) -> e) domAsses
        conj es = JAssert x (conj' es)
    in  map (\bm -> (conj $ map fst $ filter snd (zip es bm), bm)) (f (length es))
    where
        f 0 = []
        f 1 = [[False], [True]]
        f n = let bms = f (n - 1)
              in  map (False:) bms ++ map (True:) bms
        conj' [] = JEPrim (PBool False)
        conj' (e:[]) = e
        conj' (e:es) = JEBinary Or e (conj' es)

-- reply :: Reply -> Session ()
reply r = (_handler <$> get) >>= \hd -> liftIO (ePutLine hd r)

addValueMethod :: String -> JsVal -> ServeReq String
addValueMethod iname val = do
    ens <- case val of
            JVRef r -> do
                y <- lookupBinding r
                return $ "ret == " ++ y
            JVPrim pty jass -> do
                return $ prettyShow $ app jass (Name "ret")
            _ -> throwE $ "can't set attribute as " ++ show val
    fname <- fresh
    ty <- iTypeToDyType <$> inferJsValType val
    let mtd = TraitMemberMethod {
        _tmName = fname,
        _tmArgs = [],
        _tmRet  = Just ("ret", ty),
        _tmEnsures = Just ens,
        _tmRequires = Nothing
    }
    modifyTrait iname (\t -> t { _tmethods = M.insert fname mtd (_tmethods t) }) (return ())
    return fname


-- NOTE: Side-effect free
withModifiedMethod :: String -> String -> (TraitMemberMethod -> TraitMemberMethod) -> ServeReq Report -> ServeReq Report
withModifiedMethod tname fname f m = do
    withModifiedTrait tname (\t ->
        let mtds = _tmethods t in
        case M.lookup fname mtds of
            Just mtd -> t { _tmethods = M.insert fname (f mtd) mtds }
            Nothing  -> t) m

-- NOTE: has side-effect
withCopiedMethod :: String -> String -> (TraitMemberMethod -> TraitMemberMethod) ->
                    (String -> ServeReq Report) -> ServeReq Report
withCopiedMethod tname baseFname f m = do
    x <- fresh
    let newFname = baseFname ++ x
    modifyTrait tname (\t ->
        let mtds = _tmethods t in
        case M.lookup baseFname mtds of
            Just mtd -> t { _tmethods = M.insert newFname (f (mtd { _tmName = newFname })) mtds }
            Nothing  -> t) (m newFname)

modifyTrait x f m = do
    traits <- lookupTraits
    case M.lookup x traits of
        Just t -> do
            modify (\s -> s { _traits = M.insert x (f t) traits })
            m
        Nothing -> throwE "Invalid interface name"

-- NOTE: Side-effect free
withModifiedTrait :: String -> (Trait -> Trait) -> ServeReq a -> ServeReq a
withModifiedTrait x f m = do
    traits <- lookupTraits
    case M.lookup x traits of
        Just t -> do
            modify (\s -> s { _traits = M.insert x (f t) traits })
            a <- m
            modify (\s -> s { _traits = M.insert x t traits })
            return a
        Nothing -> throwE "Invalid interface name"

inferJsUnionValType :: JsUnionVal -> ServeReq IType_
inferJsUnionValType (JsUnionVal [val]) = ITySingle <$> inferJsValType val
inferJsUnionValType (JsUnionVal vals) = ITyUnion <$> mapM inferJsValType vals

inferJsValType :: JsVal -> ServeReq IType
inferJsValType = \case
    JVRef r -> do
        JsObj iname <- lookupObj r
        return $ TyInterface iname
    JVPrim pty _ -> return $ pTyToIType pty
    JVClos _ -> throwE $ "can't infer type for callback"
    JVDict _ -> throwE $ "can't infer type for dictionary"
    
nullJsRetVal :: JsValResult
nullJsRetVal = JVRPrim PTyNull [True]

getADTConsName :: String -> IType -> String
getADTConsName tyName ty = tyName ++ "." ++ toUpperFirst (prettyShow ty)

inlineAssCtx :: (String -> ServeReq ()) -> DyExpr -> AssContext
inlineAssCtx effM de jass = do
    x <- fresh
    let je = app jass (Name x)
    asse <- compileExpr je
    effM x
    tempTLM $ do
        addStmt $ SVarDef x de
        addStmt $ SAssert asse
        getSat

tempTLM :: ServeReq a -> ServeReq a
tempTLM m = do
    tlm <- _tlm <$> get
    a <- m
    modify (\s -> s { _tlm = tlm })
    return a

merge :: [[(JsImmVal, IType)]] -> [([JsImmVal], [IType])]
merge [] = [([], [])]
merge [choices] = map (\(val, ty) -> ([val], [ty])) choices
merge (choices:args) = concatMap (\(val, ty) -> map (\(vals, tys) -> (val : vals, ty: tys)) (merge args)) choices

singlify :: (JsUnionVal, IType_) -> ServeReq [(JsImmVal, IType)]
singlify (JsUnionVal [x], ITySingle ty) = return [(ImmVal x, ty)] -- no need to fiddling the type if monomorphic
singlify (JsUnionVal vals, ITyUnion tys)
    | length vals <= length tys = do
        let iname = getUnionIfaceName tys
        let consNames = map (getADTConsName iname) tys
        return $ zip (map (\(consName, val) -> ImmApp (Name consName) val)
                          (zip consNames vals))
                     (repeat (TyInterface $ Name iname))
singlify x = error $ "singlify " ++ show x ++ " failed"

data OperationOrConstructor = Op Operation | Cons Name InterfaceConstructor

oocArgs :: OperationOrConstructor -> [Argument]
oocArgs (Op op) = _imArgs op
oocArgs (Cons _ cons) = _icArgs cons

oocOptArgs :: OperationOrConstructor -> [Argument]
oocOptArgs (Op op) = _imOptArgs op
oocOptArgs (Cons _ cons) = _icOptArgs cons

oocRet :: OperationOrConstructor -> Maybe IType
oocRet (Op op) = _imRet op
oocRet (Cons iname _) = Just $ TyInterface iname

oocMEffects (Op op) = _imEffects op
oocMEffects (Cons _ cons) = _icEffects cons

data JsImmVal = ImmVal JsVal
              | ImmApp Name JsVal
              deriving Show

-- Monoid Assertion Context
type AssContext = JAssert -> ServeReq Report

type CallbackTriple = ( Int -- arity
                      , CallbackSpec -- spec
                      , Callback -- AST node
                      )

pprintDatatypes :: M.Map String [(String, DyType)] -> String
pprintDatatypes m = unlines $ map pprintDatatype (M.toList m)
    where
        pprintDatatype (tyName, constrs) = "datatype " ++ tyName ++ " = " ++ join " | " (map pprintConstr constrs)
        pprintConstr (consName, ty) = toUpperFirst consName ++ "(" ++ toUpperFirst consName ++ " : " ++ prettyShow ty ++ ")"

exprToTerm :: DyExpr -> ServeReq DyTerm
exprToTerm (DTerm tm) = return tm
exprToTerm other = do
    x <- fresh
    addStmt (SVarDef x other)
    return (DVal (DVar x))

