module Session (run) where

import Translate
import Spec
import Dafny
import JS.Type
import JS.Platform
import Model
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Char (toLower)
import qualified Data.Map as M
import Network.Simple.TCP
import Text.PrettyPrint.Leijen (pretty)
import Network.Socket (socketToHandle)
import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Dafny.Request
import Dafny.Analyze

data SessionState = SessionState {
    -- Abstract heap
    _heap :: Heap,
    -- Top-level methods
    _tlms :: M.Map String TopLevelMethod,
    -- Current compilation target
    _target :: String,
    -- Original spec
    _spec :: Spec,

    _handler :: Handle
}

initState :: Spec -> Handle -> SessionState
initState spec handler = SessionState {
    _heap = initHeap,
    _tlms = M.empty,
    _target = "",
    _spec = spec,
    _handler = handler
}

type Session = StateT SessionState IO

run :: Spec -> IO ()
run spec = do
    hSetBuffering stdout NoBuffering
    putStrLn "Engine launched, waiting for connection"
    serve (Host "localhost") "8888" $ \(sock, addr) -> do
        putStrLn $ "TCP connection established from " ++ show addr
        handler <- socketToHandle sock ReadWriteMode
        hSetBuffering handler NoBuffering
        let traits = translateSpec spec
        putStrLn "// ------ traits ------- "
        mapM_ (print . pretty) traits
        evalStateT loop (initState spec handler)

-- Emit code inside named top-level method target
withTarget :: String -> Session () -> Session TopLevelMethod
withTarget newTarget emit = do
    oldTarget <- _target <$> get
    modify (\s -> s { _target = newTarget })
    modify (\s -> s { _tlms = M.insert newTarget initTargetMethod (_tlms s) })

    emit

    modify (\s -> s { _target = oldTarget })

    tlms <- _tlms <$> get
    return $ fromJust (M.lookup newTarget tlms)
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
                    liftIO $ putStrLn "Invalid request"
                    return Nothing
    case mReply of
        Nothing -> do
            return ()
        Just reply -> do
            liftIO $ BS.hPut handler (BSL.toStrict (encode reply) `BC.snoc` '\n')
            loop

dispatch :: Command -> Session (Maybe Reply)
dispatch = \case
    -- Free (Eval e k) -> do
    --     e' <- compileExpr e
    --     val <- interpretExpr e' e
    --     handle (k val)
    CInvoke lvar x args -> do
        reply <- handleInvoke lvar x args
        return (Just reply) -- XXX: connect with Dafny
    -- Free (Assert e next) -> do
    --     handleAssert e
    --     handle next
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
handleInvoke lvar x args = do
    tlm <- withTarget "Main" $ do
        v <- compileLVar lvar
        args' <- mapM compileExpr args
        addStmt (SInvoke v (unName x) args')
    getSat tlm

getSat :: TopLevelMethod -> Session Reply
getSat tlm = do
    let src = show (pretty tlm)
    ans <- liftIO $ askDafny REST src
    case ans of
        Right Verified -> return Sat
        Right Failed -> return Unsat
        Left err -> error $ "Dafny connection error: " ++ err

compileLVar :: LVar -> Session String
compileLVar = \case
    LVal v -> case v of
        JVRef r -> lookupBinding r
        _ -> error "Unable to process invocation on non-JRef type now"
    LInterface iname -> getPlatObj iname

handleAssert :: JsExpr -> Session ()
handleAssert e = do
    e' <- compileExpr e
    addStmt (SAssert e')
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

    JCall lvar f _ -> case lvar of
        LInterface iname -> findIfaceMethodRetTy iname f
        LVal v -> case v of
            JVRef r -> do
                JsObj iname <- lookupObj r
                findIfaceMethodRetTy iname f
            _ -> error "Unable to process invocation on non-JRef type now"

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

compileExpr :: JsExpr -> Session DyExpr
compileExpr (JVal v) = DVal <$> compileJsVal v
compileExpr (JCall lvar (Name f) es) = do
    x <- compileLVar lvar
    DCall x f <$> mapM compileExpr es
compileExpr (JAccess lvar (Name attr)) = do
    x <- compileLVar lvar
    return $ DAccess x attr
compileExpr (JRel op e1 e2) = DRel op <$> compileExpr e1 <*> compileExpr e2
compileExpr (JNew i args) = do
    types <- mapM inferType args
    cons_name <- findCons i types
    v <- getPlatObj i
    args' <- mapM compileExpr args
    return (DCall v cons_name args')

{-
    DRef  -> DVar
    JVPrim -> DPrim
    -- JSeq  -> DSeq
-}

compileJsVal :: JsVal -> Session DyVal
compileJsVal (JVRef r) = DVar <$> lookupBinding r
compileJsVal (JVPrim p) = return (DPrim p)
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
    ifaces <- filterIface . _spec <$> get
    case M.lookup i ifaces of
        Just iface -> case M.lookup f (_operations iface) of
            Just method -> return method
            Nothing -> error $ "Invalid method name: " ++ show f
        Nothing -> error $ "Invalid Interface name: " ++ show i

lookupAttr :: Name -> Name -> Session (Maybe    Type)
lookupAttr i a = do
    ifaces <- filterIface . _spec <$> get
    case M.lookup i ifaces of
        Just iface -> return $ M.lookup a (_ghostAttrs iface)
        Nothing    -> error $ "Invalid Interface name: " ++ show i

iTypeToJsType :: Type -> JsType
iTypeToJsType = \case
    TyInterface x -> JTyObj x
    TyDOMString   -> JTyPrim PTyString
    TyNullable (TyInterface x) -> JTyObj x
    TyInt         -> JTyPrim PTyInt
    ty -> error $ "Can't translate Type: " ++ show ty

findCons :: Name -> [JsType] -> Session String
findCons iname argTypes = do
    ifaces <- filterIface . _spec <$> get
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

filterIface :: Spec -> M.Map Name Interface
filterIface (Spec m) = foldr f M.empty (M.toList m)
    where
        f (x, DefInterface i) im = M.insert x i im
        f _ im = im
