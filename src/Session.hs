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

import Model
import State
import Util

import Control.Monad.State
import Control.Monad (forM_)
import Control.Monad.Trans.Except

import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Char (toLower)
import Data.Atomics.Counter
import Data.Aeson
import Data.Either (partitionEithers)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import System.IO
import Text.PrettyPrint.Leijen (pretty, Pretty)

startServe = undefined

run :: IO ()
run = do
    counter <- newCounter 0
    hSetBuffering stdout NoBuffering
    logging "Engine launched, waiting for connection"
    startServe $ \handler -> do
        hSetBuffering handler NoBuffering

        snum <- incrCounter 1 counter
        logging $ "Creating session #" ++ show snum
        
        Just (CBoot (Domains m) idl) <- eGetLine handler
        preludeIDL <- readFile "prelude.idl"
        logging $ "Get domains: " ++ show m

        case parseIDL (preludeIDL ++ "\n" ++ idl) of
            Left e -> warning $ "Parse of IDL failde: " ++ show e
            Right idlAST ->
                case transDefsToSpec idlAST of
                    Left e -> warning $ "Translation of IDL failde: " ++ show e
                    Right spec ->
                        case translateSpec spec of
                            Left e -> warning $ "Translation of spec failed: " ++ e
                            Right traits -> do
                                putStrLn "// ------ traits ------- "
                                mapM_ (print . pretty) traits

                                evalStateT loop (SessionState {
                                    _heap = initHeap,
                                    _tlm  = initTargetMethod,
                                    _spec = spec,
                                    _handler = handler,
                                    _snum = snum,
                                    _traits = traits,
                                    _pDomains = M.fromList m,
                                    _namer = initNamer
                                })
                                logging $ "Ending session #" ++ show snum
    where
        initTargetMethod = TopLevelMethod {
            _tlName = "Main",
            _tlArgs = M.empty,
            _tlRequires = [],
            _tlBody = []
        }

loop :: Session ()
loop = do
    handler <- _handler <$> get
    mCmd <- liftIO $ eGetLine handler
    case mCmd of
        Just cmd -> do
            logging $ "> Request " ++ show cmd
            eErr <- runExceptT (dispatch cmd)
            case eErr of
                Right continue | continue     -> loop
                               | not continue -> return ()
                Left errMsg -> reply (InvalidReqeust errMsg) >> loop
        Nothing -> do
            warning $ "> Invalid request"
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

-- This is a non-trivial featuring, needing the enhancement of Dafny codegen module
handleSet :: LVar -> Name -> JsVal -> ServeReq ()
handleSet lvar name val = reply (InvalidReqeust "set is not supported yet")

handleNewDef :: Name -> ServeReq ()
handleNewDef iname = do
    jsRetVal <- getJsValResult (DCall (unName iname) "new_def" [])
                               (TyInterface iname)
                               (\_ -> error "Unreachable")
    reply $ Sat (jsRetVal, Nothing)

handleNewCons :: Name -> [JsUnionVal] -> ServeReq ()
handleNewCons iname uvals = do
    types <- mapM inferJsUnionValType uvals
    (consName, cons) <- lookupCons iname types
    handleUnionCall (LInterface iname) (Cons iname (Name consName) cons) uvals

inlineAssCtx :: AssContext
inlineAssCtx jass = do
    x <- fresh
    let je = app jass (Name x)
    de <- compileExpr je
    addStmt $ SAssert de
    getSat

handleGet :: LVar -> Name -> ServeReq ()
handleGet lvar aname@(Name attr) = do
    iname <- lvarToIfaceName lvar
    ty <- lookupAttr iname aname
    x <- compileLVar lvar
    jsValRet <- getJsValResult (DAccess x attr) ty inlineAssCtx
    reply $ Sat (jsValRet, Nothing)

handleUnionCall :: LVar -> OperationOrConstructor -> [JsUnionVal] -> ServeReq ()
handleUnionCall lvar ooc uvals = do
    -- regenerate mono-calls
    let pairs = zip uvals (map _argTy (oocArgs ooc ++ oocOptArgs ooc))

    let calls = merge $ map singlify pairs

    forM_ calls $ \(vals, argtys) -> do
        -- compile non-optional arguments
        handleSingleCall lvar ooc vals argtys

merge :: [[(JsImmVal, IType)]] -> [([JsImmVal], [IType])]
merge [] = []
merge [choices] = map (\(val, ty) -> ([val], [ty])) choices
merge (choices:args) = concatMap (\(val, ty) -> map (\(vals, tys) -> (val : vals, ty: tys)) (merge args)) choices

singlify :: (JsUnionVal, IType_) -> [(JsImmVal, IType)]
singlify (JsUnionVal [x], ITySingle ty) = [(ImmVal x, ty)] -- no need to fiddling the type if monomorphic
singlify (JsUnionVal vals, ITyUnion tys)
    | length vals == length tys =
        let iname = getUnionIfaceName tys
            consNames = map (getADTConsName iname) tys
        in  zip (map (\(consName, val) -> ImmApp consName val) (zip consNames vals)) (repeat (TyInterface iname))
singlify x = error $ "singlify " ++ show x ++ " failed"

data OperationOrConstructor = Op Operation | Cons Name Name InterfaceConstructor

oocArgs :: OperationOrConstructor -> [Argument]
oocArgs (Op op) = _imArgs op
oocArgs (Cons _ _ cons) = _icArgs cons

oocOptArgs :: OperationOrConstructor -> [Argument]
oocOptArgs (Op op) = _imOptArgs op
oocOptArgs (Cons _ _ cons) = _icOptArgs cons

oocName :: OperationOrConstructor -> Name
oocName (Op op) = _imName op
oocName (Cons _ consName _) = consName

oocRet :: OperationOrConstructor -> Maybe IType
oocRet (Op op) = _imRet op
oocRet (Cons iname _ _) = Just $ TyInterface iname

data JsImmVal = ImmVal JsVal
              | ImmApp Name JsVal

handleSingleCall :: LVar -> OperationOrConstructor -> [JsImmVal] -> [IType] -> ServeReq ()
handleSingleCall lvar ooc vals tys = do
    let nargs = length (oocArgs ooc) + length (oocOptArgs ooc)
    let nopts = length (oocOptArgs ooc)
    let argNames = (map _argName $ oocArgs ooc ++ oocOptArgs ooc)
    (args, cbs) <- partitionEithers <$>
                       forM (zip3 vals tys argNames)
                            (\(val, ty, argn) -> do
                                let f = \case
                                            JVClos n -> do
                                                let cbspec = queryCallbackSpec ooc argn
                                                cb <- lookupCb ty
                                                return $ Right (n, cbspec, cb)
                                            val -> Left <$> compileNonCbJsVal val ty
                                case val of
                                    ImmVal val -> f val
                                    ImmApp fname val -> do
                                        lr <- f val
                                        case lr of
                                            Left e -> return $ Left $ DApp (unName fname) [e]
                                            Right _ -> return lr)
                                
    let nnonopt = nargs - nopts
    let args' = take nnonopt args ++
                map DSome (drop nnonopt args) ++
                take (nargs - length args) (repeat DNone)

    -- comopile callback replies
    cbsret <- if cbs == [] then return Nothing else Just <$> compileCbs lvar ooc args' cbs

    -- compile value replies
    x <- compileLVar lvar
    let fname = unName (oocName ooc)
    case oocRet ooc of
        Nothing -> do -- invocation, no return value
            ret <- checkInvocation x fname args'
            if ret
                then reply $ Sat (nullJsRetVal, cbsret)
                else reply $ Unsat "Invalid invocation"
        Just retty -> do -- evaluation, with return value
            jsRetVal <- getJsValResult (DCall x fname args') retty inlineAssCtx
            reply $ Sat (jsRetVal, cbsret)

-- Monoid Assertion Context
type AssContext = JAssert -> ServeReq Report

-- If nothing, means failed
getJsValResult :: DyExpr -> IType -> AssContext -> ServeReq JsValResult
getJsValResult dyexpr ty ctx =
    case ty of
        TyInterface iname -> getObjResult iname
        TyNullable ty -> getJsValResult dyexpr ty ctx
        TyBuiltIn iname -> getObjResult iname
        TyAny -> getObjResult (Name "Any")
        TyObject -> getObjResult (Name "Object")
        TyBoolean -> getPrimResult PTyBool dyexpr ctx
        TyInt -> getPrimResult PTyInt dyexpr ctx
        TyFloat -> getPrimResult PTyNumber dyexpr ctx
        TyDOMString -> getPrimResult PTyString dyexpr ctx
        TyArray _ -> throwE $ "array is not supported yet"
    where
        getObjResult iname = do
            (r, vname) <- allocOnHeap iname
            addStmt (SVarDef vname dyexpr)
            return $ (JVRRef r)

getPrimResult :: PrimType -> DyExpr -> AssContext -> ServeReq JsValResult
getPrimResult pty de ctx = do
    domainMap <- _pDomains <$> get
    let Just domains = M.lookup pty domainMap
    let assertions = domainsToAssertions domains
    (_, flags) <- head <$> flip filterM assertions (\(assert, _) -> reportToBool <$> ctx assert)
    return (JVRPrim pty flags)


type CallbackTriple = ( Int -- arity
                      , CallbackSpec -- spec
                      , Callback -- AST node
                      )

compileCbs :: LVar -> OperationOrConstructor -> [DyExpr] -> [CallbackTriple] -> ServeReq JsCallbackResult
compileCbs lvar ooc noncbargs cbs = do
    iname <- unName <$> lvarToIfaceName lvar
    let fname = unName (oocName ooc)
    let checkSat = do
                        f <- fresh
                        x <- compileLVar lvar
                        lhs <- fresh
                        addStmt (SVarDef lhs (DCall x fname noncbargs))
                        getSat

    mBranchs <- forM cbs $ \(n, CallbackSpec _ reqe withEs, cb) -> do

        eReport <- withModifiedMethod iname fname (andRequires reqe) checkSat

        if reportToBool eReport
            then do
                jsRetVals <- forM (zip withEs (_cArgs cb)) $ \(withE, arg) -> do
                    let ITySingle ty = _argTy arg
                    getJsValResult (DStrRepr withE) ty $ \(JAssert name je) -> do
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
    TyInterface i -> compileIfaceOrDict val i
    TyDOMString -> compilePrim val PTyString
    TyInt -> compilePrim val PTyInt
    TyFloat -> compilePrim val PTyNumber
    TyAny -> compileIfaceOrDict val (Name "Any")
    TyNullable ty -> compileNonCbJsVal val ty
    TyBoolean -> compilePrim val PTyBool
    TyObject -> compileIfaceOrDict val (Name "Object")
    TyBuiltIn i -> compileIfaceOrDict val i
    TyArray _ -> throwE "array type is not supported yet"

compileIfaceOrDict :: JsVal -> Name -> ServeReq DyExpr
compileIfaceOrDict jsval iname = do
    case lookupDefinition iname of
        Nothing -> throwE $ "Invalid interface name: " ++ show iname
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
        then (DVal . DVar) <$> lookupBinding jref
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
                                Just ty -> compileNonCbJsVal jsval (dyTypeToIType ty)

    return (DCall (unName (_dname dict)) "new" dyexprs)

compileDict otherval _ = throwE $ "Invalid jsval for dictionary: " ++ show otherval

compilePrim :: JsVal -> PrimType -> ServeReq DyExpr
compilePrim (JVPrim pty assert) _ = do
    x <- fresh
    let je = app assert (Name x)
    de <- compileExpr je
    addArg x (pTyToDTy pty)
    addRequire de
    return (DVal (DVar x))

compileLVar :: LVar -> ServeReq String
compileLVar = \case
    LRef r -> lookupBinding r
    LInterface iname -> lookupPlatObj iname

compileExpr :: JsExpr -> ServeReq DyExpr
compileExpr (JEVar x) = return (DVal $ DVar (unName x))
compileExpr (JEPrim prim) = return $ DVal (DPrim prim)
compileExpr (JEBinary op e1 e2) = do
    DRel op <$> compileExpr e1 <*> compileExpr e2
compileExpr other = throwE $ "Can't compile Expr " ++ show other

-- Misc

andRequires :: String -> TraitMemberMethod -> TraitMemberMethod
andRequires r m = modifyRequires m (\case
                                        Just s  -> Just ("(" ++ s ++ ") && " ++ r)
                                        Nothing -> Just r)

letBindRequires :: String -> DyExpr -> TraitMemberMethod -> TraitMemberMethod
letBindRequires x e m = modifyRequires m (fmap (\s -> "var " ++ x ++ " := " ++ prettyShow e ++ "; (" ++ s ++ ")"))

modifyRequires :: TraitMemberMethod -> (Maybe String -> Maybe String) -> TraitMemberMethod
modifyRequires m f = m { _tmRequires = f (_tmRequires m) }

queryCallbackSpec :: OperationOrConstructor -> Name -> CallbackSpec
queryCallbackSpec ooc x = 
    case ooc of
        Op op -> head $ filter (\(CallbackSpec x' _ _) -> x' == x) (_imCbs op)
        Cons _ _ _ -> error "can't query constructor for callback"

getSat :: ServeReq Report
getSat = do
    tlm <- _tlm <$> get
    traits <- lookupTraits
    modify (\s -> s { _names = M.empty, _traitsNew = Nothing })
    let src = unlines (map (show . pretty) $ M.elems traits) ++ "\n" ++ show (pretty tlm)
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
    Failed   -> False

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

-- NOTE: Side-effect free
withModifiedMethod :: String -> String -> (TraitMemberMethod -> TraitMemberMethod) -> ServeReq Report -> ServeReq Report
withModifiedMethod tname fname f m = do
    withModifiedTrait tname (\t ->
        let mtds = _tmethods t in
        case M.lookup fname mtds of
            Just mtd -> t { _tmethods = M.insert fname (f mtd) mtds }
            Nothing  -> t) m

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

pTyToIType :: PrimType -> IType
pTyToIType = \case
    PTyNull -> error "can't infer type for null value"
    PTyNumber -> TyFloat
    PTyInt -> TyInt
    PTyString -> TyDOMString
    PTyBool -> TyBoolean


checkInvocation = undefined
    
nullJsRetVal :: JsValResult
nullJsRetVal = undefined

lookupDefinition :: Name -> Maybe Definition
lookupDefinition = undefined


dyTypeToIType = undefined


pTyToDTy = undefined

