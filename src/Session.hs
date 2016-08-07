module Session (run, andRequires) where

import Language.Dafny.Translate
import Language.Dafny.AST
import Language.Dafny.Request
import Language.Dafny.Analyze

import Language.XWIDL.Spec

import Language.JS.Type
import Language.JS.Platform

import Model

import Control.Monad.State
import Control.Monad (forM_)

import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Char (toLower)
import Data.Atomics.Counter
import Data.Aeson

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Text.PrettyPrint.Leijen (pretty)

import Network.Simple.TCP
import Network.Socket (socketToHandle)

import System.IO

data SessionState = SessionState {
    -- Abstract heap
    _heap :: Heap,
    -- Top-level methods
    _tlms :: M.Map String TopLevelMethod,
    -- Current compilation target
    _target :: String,
    -- Original spec
    _spec :: Spec,
    -- Socket handle
    _handler :: Handle,
    -- Session number
    _snum :: Int,
    -- Traits text
    _traits :: M.Map String Trait,
    -- New traits, which will be used as part of code emission if avaiable
    _traitsNew :: Maybe (M.Map String Trait),
    -- Primitive type domains
    _pDomains :: M.Map PrimType [JAssert],
    -- unique namer
    _namer :: Namer,
    -- name table
    _names :: M.Map Name String
}

type Session = StateT SessionState IO

run :: Spec -> IO ()
run spec =
    case translateSpec spec of
        Left e -> putStrLn $ "Translation of spec failed: " ++ e
        Right traits -> do
            putStrLn "// ------ traits ------- "
            mapM_ (print . pretty) traits
            counter <- newCounter 0
            hSetBuffering stdout NoBuffering
            putStrLn "Engine launched, waiting for connection"
            serve (Host "localhost") "8888" $ \(sock, addr) -> do
                snum <- incrCounter 1 counter
                putStrLn $ "Creating session #" ++ show snum
                putStrLn $ "TCP connection established from " ++ show addr
                handler <- socketToHandle sock ReadWriteMode
                hSetBuffering handler NoBuffering
                Just (Domains m) <- eGetLine handler
                putStrLn $ "Get domains: " ++ show m
                evalStateT loop (SessionState {
                    _heap = initHeap,
                    _tlms = M.empty,
                    _target = "",
                    _spec = spec,
                    _handler = handler,
                    _snum = snum,
                    _traits = traits,
                    _traitsNew = Nothing,
                    _pDomains = M.fromList m,
                    _namer = initNamer,
                    _names = M.empty
                })
                putStrLn $ "Ending session #" ++ show snum

-- Emit code inside named top-level method target
withTarget :: String -> Session a -> Session (TopLevelMethod, a)
withTarget newTarget emit = do
    oldTarget <- _target <$> get
    modify (\s -> s { _target = newTarget })
    modify (\s -> s { _tlms = M.insert newTarget initTargetMethod (_tlms s) })

    a <- emit

    modify (\s -> s { _target = oldTarget })

    tlms <- _tlms <$> get
    return (fromJust (M.lookup newTarget tlms), a)
    where
        initTargetMethod = TopLevelMethod {
            _tlName = newTarget,
            _tlArgs = M.empty,
            _tlRequires = [],
            _tlBody = []
        }

loop :: Session ()
loop = do
    handler <- _handler <$> get
    line <- BSL.fromStrict <$> liftIO (BS.hGetLine handler)
    mReply <- case (decode line :: Maybe Command) of
                Just cmd -> do
                    liftIO $ putStrLn ("// [REQ] " ++ show cmd)
                    dispatch cmd
                Nothing  -> do
                    liftIO $ putStrLn $ "Invalid request: " ++ show line
                    return Nothing
    case mReply of
        Nothing -> return ()
        Just r -> do
            liftIO $ BS.hPut handler (BSL.toStrict (encode r) `BC.snoc` '\n')
            loop

reply :: Reply -> Session ()
reply r = (_handler <$> get) >>= \hd -> liftIO (BS.hPut hd (BSL.toStrict (encode r) `BC.snoc` '\n'))

dispatch :: Command -> Session (Maybe Reply)
dispatch = \case
    CInvoke lvar x args -> Just <$> handleInvoke lvar x args
    -- CEval e -> Just <$> handleEval e
    CCall lvar f args -> Just <$> handleCall lvar f args
    CAssert e -> Just <$> handleAssert e
    CEnd -> return Nothing
{-
    While assert/invoke are *ephemeral* (not really, since ghost states might be affected),
    eval *could* have persistent effect if some constructive operation is involved, like
    new expression or other constructive platform methods.

    By introducing a type inferencer, we can know if the JsExpr is of constructive type,
    if so, some side effects should be performed.

    p.s. type inferencer could also be useful in the constructor matching as well!
-}
interpretExpr :: DyExpr -> JsExpr -> Session JsVal
interpretExpr de je = inferType je >>= \case
    JTyObj iname -> do
        (r, vname) <- allocOnHeap iname
        addStmt (SVarDef vname de)
        return (JVRef r)
    JTyPrim prim -> case je of
        JVal val -> return val
        -- XXX: This check is not proven to be complete
        JNew _ _ -> error "Impossible happens"
        -- XXX: Below overly-approximation might be problematic
        _ -> return $ JVPrim (defaultPrim prim)

handleInvoke :: LVar -> Name -> [JsExpr] -> Session Reply
handleInvoke lvar fname args = do
    (tlm, _) <- withTarget "Main" $ do
        v <- compileLVar lvar
        op <- lookupOperationWithLvar fname lvar
        let argtys = map _argTy (_imArgs op)
        args' <- mapM (uncurry compileExpr) (zip args argtys)
        addStmt (SInvoke v (unName fname) args')
    reportToReply <$> getSat tlm

reportToReply :: Report -> Reply
reportToReply = \case
    Verified -> Sat Nothing
    Failed   -> Unsat

reportToBool :: Report -> Bool
reportToBool = \case
    Verified -> True
    Failed   -> False

-- handleEval :: JsExpr -> Session Reply
-- handleEval e = inferType e >>= \case
--     JTyPrim pty -> handlePrimEval pty e
--     JTyObj _    -> handleObjEval e
--     _           -> error "Invalid eval"

-- handlePrimEval :: PrimType -> JsExpr -> Session Reply
-- handlePrimEval pty e = do
--     liftIO $ putStrLn "handlePrimEval"
--     domainMap <- _pDomains <$> get
--     let Just domains = M.lookup pty domainMap
--     let assertions = domainsToAssertions domains
--     (_, flags) <- head <$> flip filterM assertions
--                           (\(assert, _) -> do
--                                 (tlm, _) <- withTarget "Main" $ do
--                                     de <- compileExpr e ()
--                                     compileAssert de assert
--                                 reportToBool <$> getSat tlm)
--     return (Replies pty flags)

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
        conj' [] = JVal (JVPrim (PBool False))
        conj' (e:[]) = e
        conj' (e:es) = JRel Or e (conj' es)

-- handleObjEval :: JsExpr -> Session Reply
-- handleObjEval e = do
--     liftIO $ putStrLn "handleObjEval"
--     (tlm, r) <- withTarget "Main" $ do
--         e' <- compileExpr e
--         JVRef r <- interpretExpr e' e
--         return r
--     report <- getSat tlm
--     case report of
--         Verified -> return (Sat (Just r))
--         Failed -> return Unsat

-- compileAssert :: DyExpr -> JAssert -> Session ()
-- compileAssert de (JAssert n@(Name x) e) = do
--     prefix <- fresh
--     let vname = prefix ++ x
--     addName n vname
--     addStmt $ SVarDef vname de
--     e' <- compileExpr e
--     addStmt $ SAssert e'

getSat :: TopLevelMethod -> Session Report
getSat tlm = do
    traits <- getTraits
    modify (\s -> s { _names = M.empty, _traitsNew = Nothing })
    let src = unlines (map (show . pretty) $ M.elems traits) ++ "\n" ++ show (pretty tlm)
    liftIO $ putStrLn ("Getting sat from REST...tlm: \n" ++ src)
    ans <- liftIO $ askDafny (Local "/home/zhangz/xwidl/dafny/Binaries") src
    case ans of
        Right ret -> do
            liftIO $ putStrLn ("Got sat: " ++ show ret)
            return ret
        Left err -> error $ "Dafny connection error: " ++ err

getTraits = do
    tn <- _traitsNew <$> get
    case tn of
        Just tn -> return tn
        Nothing -> _traits <$> get

compileLVar :: LVar -> Session String
compileLVar = \case
    LVal v -> case v of
        JVRef r -> lookupBinding r
        _ -> error "Unable to process invocation on non-JRef type now"
    LInterface iname -> getPlatObj iname

handleAssert :: JsExpr -> Session Reply
handleAssert e = do
    (tlm, _) <- withTarget "Main" $ do
        e' <- compileExpr e TyBoolean
        addStmt (SAssert e')
    reportToReply <$> getSat tlm

{-
    JVal  -> DyVal
    JCall -> DCall
    JRel  -> DRel
    JNew  -> DCall
-}

inferType :: JsExpr -> Session JsType
inferType = \case
    JVal val -> case val of
        JVRef r -> do
            JsObj iname <- lookupObj r
            return (JTyObj iname)
        JVPrim prim -> return (JTyPrim $ inferPrimType prim)

    -- JCall lvar f _ -> case lvar of
    --     LInterface iname -> findIfaceMethodRetTy iname f
    --     LVal v -> case v of
    --         JVRef r -> do
    --             JsObj iname <- lookupObj r
    --             findIfaceMethodRetTy iname f
    --         _ -> error "Unable to process invocation on non-JRef type now"

    JAccess lvar attr -> case lvar of
        LInterface iname -> findIfaceAttrTy iname attr
        LVal v -> case v of
            JVRef r -> do
                JsObj iname <- lookupObj r
                findIfaceAttrTy iname attr
            _ -> error "Unable to process access on non-JRef type now"

    JRel _ _ _ -> return (JTyPrim PTyBool)
    JNew iname _ -> return (JTyObj iname)
    where
        findIfaceMethodRetTy iname fname = do
            method <- lookupOperation iname fname
            case _imRet method of
                Just ty -> return (iTypeToJsType ty)
                Nothing -> return (JTyPrim PTyNull)
        findIfaceAttrTy iname aname = do
            mAttrTy <- lookupAttr iname aname
            case mAttrTy of
                Just ty -> return (iTypeToJsType ty)
                Nothing -> return (JTyPrim PTyNull)

handleCall lvar fn@(Name f) es = do
    op <- lookupOperationWithLvar fn lvar
    ifM (hasCallbackArgs op)
        (compileCallWithCbs lvar op es)
        (return () -- do something
            -- (tlm, _) <- withTarget "Main" $ do
            --     x <- compileLVar lvar
            --     DCall x f <$> mapM compileExpr es
            --     reportToReply <$> getSat tlm
            )
    return (Sat Nothing)

compileExpr :: JsExpr -> Type -> Session DyExpr

compileExpr (JVal v) ty = DVal <$> compileJsVal v ty

compileExpr (JInterface i) (TyInterface i')
    | i == i'   = DVal . DVar <$> compileLVar (LInterface i)
    | otherwise = error "compileExpr: iface name doesn't match"

compileExpr (JAccess lvar (Name attr)) _ = do
    x <- compileLVar lvar
    return $ DAccess x attr

compileExpr (JRel op e1 e2) TyBoolean = do
    ty1 <- jsTypeToIType <$> inferType e1
    ty2 <- jsTypeToIType <$> inferType e2
    if ty1 == ty2 then
        DRel op <$> compileExpr e1 ty1 <*> compileExpr e2 ty2
        else error "biop diff type"
compileExpr (JNew i args) (TyInterface i') | i == i' = do
    types <- mapM inferType args
    cons_name <- findCons i types
    v <- getPlatObj i
    args' <- mapM (uncurry compileExpr) (zip args $ map jsTypeToIType types)
    return (DCall v cons_name args')

hasCallbackArgs :: Operation -> Session Bool
hasCallbackArgs op = or <$> mapM (\(Argument _ ty _) -> isJust <$> lookupCbTy ty) (_imArgs op)

compileCallWithCbs lvar op es = do
    let fname = (unName $ _imName op)
    iname <- lvarToIfaceName lvar
    (es', cbs) <- foldM (\(es, cbs) (e, Argument argn argty _)-> do
                            mCb <- lookupCbTy argty
                            case mCb of
                                Just cb -> do
                                    let cbspec = queryCallbackSpec op argn
                                    return (es, cbs ++ [(e, cbspec, cb)])
                                Nothing -> return (es ++ [(e, argty)], cbs))
                        ([], [])
                        (zip es (_imArgs op))
    cbmReplies <- forM cbs $ \(JEClos n, CallbackSpec _ reqe withEs, cb) -> do
        -- first emit a method checking if this control-flow can go
        modifyMethod (unName iname) fname (andRequires reqe)
        f <- fresh
        (tlm, _) <- withTarget ("Main_" ++ f) $ do
            x <- compileLVar lvar
            de <- DCall x fname <$> mapM (uncurry compileExpr) es'
            fx <- fresh
            addStmt (SVarDef fx de)
        eReport <- getSat tlm
        if reportToBool eReport
            then do -- if can go
                    let cbArgtys = map (\(Argument _ ty _) -> iTypeToJsType ty) (_cArgs cb)
                    replies <- forM (zip withEs cbArgtys) $ \(withE, argty) -> case argty of
                                    JTyObj iname -> do
                                        (r, vname) <- allocOnHeap iname
                                        addStmt (SVarDef vname (DStrRepr withE))
                                        return (Sat (Just r))
                                    JTyPrim pty -> do
                                            domainMap <- _pDomains <$> get
                                            let Just domains = M.lookup pty domainMap
                                            let assertions = domainsToAssertions domains
                                            (_, flags) <- head <$> flip filterM assertions
                                                                (\(JAssert name je, _) -> do
                                                                        je' <- compileExpr je TyBoolean
                                                                        let x = DVal (DVar (unName name))
                                                                        modifyMethod (unName iname) fname
                                                                                     ( letBindRequires (unName name) (DStrRepr withE)
                                                                                     . andRequires (prettyShow je'))
                                                                        f <- fresh
                                                                        (tlm, _) <- withTarget ("Main_" ++ f) $ do
                                                                            x <- compileLVar lvar
                                                                            de <- DCall x fname <$> mapM (uncurry compileExpr) es'
                                                                            fx <- fresh
                                                                            addStmt (SVarDef fx de)
                                                                        reportToBool <$> getSat tlm)
                                            return (Replies pty flags)

                    return (Just (reportToReply eReport, replies))
            else return Nothing -- if can't

    reply (ReplyCallback cbmReplies)

andRequires :: String -> TraitMemberMethod -> TraitMemberMethod
andRequires r m = modifyRequires m (\case
                                        Just s  -> Just ("(" ++ s ++ ") && " ++ r)
                                        Nothing -> Just r)

letBindRequires :: String -> DyExpr -> TraitMemberMethod -> TraitMemberMethod
letBindRequires x e m = modifyRequires m (fmap (\s -> "var " ++ x ++ " := " ++ prettyShow e ++ "; (" ++ s ++ ")"))

modifyRequires :: TraitMemberMethod -> (Maybe String -> Maybe String) -> TraitMemberMethod
modifyRequires m f = m { _tmRequires = f (_tmRequires m) }

lookupCbTy :: Type -> Session (Maybe Callback)
lookupCbTy (TyInterface n) = do
    cbs <- _cbs . _spec <$> get
    case M.lookup n cbs of
        Nothing -> return Nothing
        Just cb -> return (Just cb)
lookupCbTy _ = return Nothing

modifyMethod :: String -> String -> (TraitMemberMethod -> TraitMemberMethod) -> Session ()
modifyMethod tname fname f = do
    modifyTrait tname (\t ->
        let mtds = _tmethods t in
        case M.lookup fname mtds of
            Just mtd -> t { _tmethods = M.insert fname (f mtd) mtds }
            Nothing  -> t)

modifyTrait :: String -> (Trait -> Trait) -> Session ()
modifyTrait x f = do
    traits <- getTraits
    case M.lookup x traits of
        Just t -> modify (\s -> s { _traitsNew = Just (M.insert x (f t) traits)})
        Nothing -> return ()

queryCallbackSpec :: Operation -> Name -> CallbackSpec
queryCallbackSpec op x = head $ filter (\(CallbackSpec x' _ _) -> x' == x) (_imCbs op)

lvarToIfaceName = \case
    LInterface iname -> return iname
    LVal v -> case v of
        JVRef r -> do
            JsObj iname <- lookupObj r
            return iname

lookupOperationWithLvar fname lvar =
    lvarToIfaceName lvar >>= \iname -> lookupOperation iname fname

{-
    DRef  -> DVar
    JVPrim -> DPrim
    -- JSeq  -> DSeq
-}

compileJsVal :: JsVal -> Type -> Session DyVal
compileJsVal v ty = case v of
    JVRef r  -> DVar <$> lookupBinding r
    JVPrim p -> return (DPrim p)
    JVVar n -> do
        names <- _names <$> get
        case M.lookup n names of
            Just x -> return $ DVar x
            Nothing -> do
                warning $ "Invalid name in JsVal: " ++ show n ++ ", fall back to direct name"
                return (DVar (unName n))
    JVDict dict ->
        case ty of
            TyInterface i -> do
                dicts <- _dicts . _spec <$> get
                let Just dict = M.lookup i dicts
                -- let names = dict TODO: do some checking here
                cons_name <- findCons i []
                platv <- getPlatObj i
                vname <- fresh
                addStmt (SVarDef vname (DCall platv cons_name []))
                return (DVar vname)
            _ -> error "Unable to handle dict"
    other    -> error $ "Can't compile JsVal: " ++ show other

-- compileJsVal (JSeq vs) = DSeq <$> mapM compileJsVal vs

updateTarget :: (TopLevelMethod -> TopLevelMethod) -> Session ()
updateTarget f = modify (\s -> s { _tlms = M.update (Just . f) (_target s) (_tlms s) })

getTarget :: Session TopLevelMethod
getTarget = do
    s <- get
    case M.lookup (_target s) (_tlms s) of
        Just tlm -> return tlm
        Nothing  -> error "Invalid current target"

addArg :: String -> DyType -> Session ()
addArg x ty = updateTarget (\m -> m { _tlArgs = M.insert x ty (_tlArgs m) })

getPlatObj :: Name -> Session String
getPlatObj iname = do
    args <- _tlArgs <$> getTarget
    let vname = toLowerFirst (unName iname)
    case M.lookup vname args of
        Just _  -> return vname
        Nothing -> do
            addArg vname (DTyClass (unName iname))
            addRequire (DRel NotEqual (DVal (DVar vname)) (DVal (DPrim PNull)))
            return vname

addRequire :: DyExpr -> Session ()
addRequire e =  updateTarget (\m -> m { _tlRequires = e : _tlRequires m })

addStmt :: Stmt -> Session ()
addStmt s = updateTarget (\m -> m { _tlBody = _tlBody m ++ [s] })

addName :: Name -> String -> Session ()
addName n x = modify (\s -> s { _names = M.insert n x (_names s)})

allocOnHeap :: Name -> Session (JRef, String)
allocOnHeap iname = do
    h <- _heap <$> get
    let (r, h') = alloc (JsObj iname) h
    modify (\s -> s { _heap = h' })
    let vname = toLowerFirst (unName iname) ++ "_" ++ show (unJRef r)
    return (r, vname)

toLowerFirst :: String -> String
toLowerFirst (x : xs) = toLower x : xs
toLowerFirst [] = []

lookupObj :: JRef -> Session JsObj
lookupObj (JRef r) = do
    h <- _heap <$> get
    case M.lookup r (_mapping h) of
        Just o  -> return o
        Nothing -> error $ "Can't find ref " ++ show r ++ "in heap"

lookupBinding :: JRef -> Session String
lookupBinding r = do
    JsObj iname <- lookupObj r
    return (toLowerFirst (unName iname) ++ "_" ++ show (unJRef r))

lookupOperation :: Name -> Name -> Session Operation
lookupOperation i f = do
    ifaces <- _ifaces . _spec <$> get
    case M.lookup i ifaces of
        Just iface -> case M.lookup f (_operations iface) of
            Just method -> return method
            Nothing -> error $ "Invalid method name: " ++ show f
        Nothing -> error $ "Invalid Interface name: " ++ show i

lookupAttr :: Name -> Name -> Session (Maybe Type)
lookupAttr i a = do
    ifaces <- _ifaces . _spec <$> get
    case M.lookup i ifaces of
        Just iface -> return $ M.lookup a (_ghostAttrs iface)
        Nothing    -> error $ "Invalid Interface name: " ++ show i

iTypeToJsType :: Type -> JsType
iTypeToJsType = \case
    TyInterface x -> JTyObj x
    TyDOMString   -> JTyPrim PTyString
    TyNullable (TyInterface x) -> JTyObj x
    TyInt         -> JTyPrim PTyInt
    TyFloat       -> JTyPrim PTyDouble
    ty -> error $ "Can't translate Type: " ++ show ty


jsTypeToIType :: JsType -> Type
jsTypeToIType = \case
    JTyObj x -> TyInterface x
    JTyPrim PTyString -> TyDOMString
    JTyObj x -> TyNullable (TyInterface x)
    JTyPrim PTyInt -> TyInt
    JTyPrim PTyDouble -> TyFloat
    ty -> error $ "Can't translate Type: " ++ show ty

findCons :: Name -> [JsType] -> Session String
findCons iname argTypes = do
    ifaces <- _ifaces . _spec <$> get
    case M.lookup iname ifaces of
        Just iface -> do
            let consTypes = zip [(0 :: Int)..] (map _icArgs (_constructors iface))
            case filter (match . snd) consTypes of
                (i, _):_ -> return $ "new_" ++ show i
                _ -> error $ "Failed to find a proper constructor: " ++ show argTypes
        Nothing -> error $ "Invalid Interface name: " ++ show iname
    where
        match :: [Argument] -> Bool
        match consTypes =
            if length consTypes == length argTypes then
                let consTypes' = map (iTypeToJsType . (\(Argument _ ty _) -> ty)) consTypes
                in  and (map (uncurry typeEquiv) (zip consTypes' argTypes))
            else False

typeEquiv :: JsType -> JsType -> Bool
typeEquiv (JTyPrim PTyNull) (JTyObj _) = True
typeEquiv (JTyObj _) (JTyPrim PTyNull) = True
typeEquiv t1 t2 = t1 == t2

fresh :: Session String
fresh = do
    namer <- _namer <$> get
    let (x, namer') = freshName namer
    modify (\s -> s { _namer = namer' })
    return x

ptyToDyType :: PrimType -> DyType
ptyToDyType = \case
    PTyInt       -> DTyInt
    PTyDouble    -> DTyReal
    PTyString    -> DTyString
    PTyBool      -> DTyBool
    other        -> error $ "can't translate " ++ show other ++ " to DyType"
    -- XXX: for null, we can initialize an empty Object class

-- Namer

data Namer = Namer Int

initNamer :: Namer
initNamer = Namer 0

freshName :: Namer -> (String, Namer)
freshName (Namer x) = ("fresh_" ++ show x, Namer (x + 1))

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM x a b = x >>= \x -> if x then a else b


prettyShow = show . pretty

warning s = liftIO (putStrLn $ "[WARNING] " ++ s)
