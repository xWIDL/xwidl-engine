module State where

import Model
import Language.XWIDL.Spec
import Language.Dafny.AST
import Language.JS.Type
import Language.JS.Platform
import qualified Language.WebIDL.AST as W

import System.IO

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

import Util

import qualified Data.Map as M


-- Per-session persistent state
data SessionState = SessionState {
    -- Abstract heap
    _heap :: Heap,
    -- Top-level method
    _tlm :: TopLevelMethod,
    -- Original spec
    _spec :: Spec,
    -- Classs text
    _traits :: M.Map String Class,
    -- New traits, which will be used as part of code emission if avaiable
    _traitsNew :: Maybe (M.Map String Class),
    _datatypes :: M.Map String [(String, DyType)],
    -- Primitive type domains
    _pDomains :: M.Map PrimType [JAssert],
    -- unique namer
    _namer :: Namer,
    _prelude :: String,
    -- name table
    _names :: M.Map Name String
}

type Session = StateT SessionState IO

type ServeReq = ExceptT String Session

-- Lookups

lookupObj :: JRef -> ServeReq JsObj
lookupObj (JRef r) = do
    h <- _heap <$> get
    case M.lookup r (_mapping h) of
        Just o  -> return o
        Nothing -> throwE $ "Can't find ref " ++ show r ++ "in heap"

lookupBinding :: JRef -> ServeReq String
lookupBinding r = do
    JsObj iname <- lookupObj r
    return (toLowerFirst (unName iname) ++ "_" ++ show (unJRef r))

lookupOperation :: Name -> Name -> ServeReq Operation
lookupOperation i f = do
    ifaces <- _ifaces . _spec <$> get
    case M.lookup i ifaces of
        Just iface -> case M.lookup f (_operations iface) of
            Just method -> return method
            Nothing -> throwE $ "Invalid method name: " ++ show f
        Nothing -> throwE $ "Invalid Interface name: " ++ show i

lookupAttr :: Name -> Name -> ServeReq IType
lookupAttr i a = do
    ifaces <- _ifaces . _spec <$> get
    case M.lookup i ifaces of
        Just iface ->
            case M.lookup a (_ghostAttrs iface `M.union` _attrs iface) of
                Just ty -> return ty
                Nothing -> throwE $ "Invalid attribute " ++ show a ++ " of interface " ++ show i
        Nothing -> throwE $ "Invalid Interface name: " ++ show i

lookupCons :: Name -> [IType_] -> ServeReq InterfaceConstructor
lookupCons iname argTypes = do
    ifaces <- _ifaces . _spec <$> get
    case M.lookup iname ifaces of
        Just iface -> do
            let InterfaceConstructors conss = _constructors iface
            let args = zip [(0 :: Int)..] (map _icArgs conss)
            case filter (match . snd) args of
                (i, _):_ -> return (conss !! i)
                _ -> throwE $ "Failed to find a proper constructor: " ++ show argTypes
        Nothing -> throwE $ "Invalid Interface name: " ++ show iname
    where
        match :: [Argument] -> Bool
        match args =
            if length args == length argTypes then
                let argTypes' = map (\(Argument _ ty _) -> ty) args
                in  and (map (uncurry (==)) (zip argTypes' argTypes))
            else False

lookupPlatObj :: Name -> ServeReq String
lookupPlatObj iname = do
    args <- _tlArgs . _tlm <$> get
    let vname = toLowerFirst (unName iname)
    case M.lookup vname args of
        Just _  -> return vname
        Nothing -> do
            addArg vname (DTyClass (unName iname))
            addRequire (DTerm (DRel NotEqual (DVal (DVar vname))
                                             (DVal (DPrim PNull))))
            return vname

lookupOperationWithLvar fname lvar =
    lvarToIfaceName lvar >>= \iname -> lookupOperation iname fname

lookupCb :: IType -> ServeReq Callback
lookupCb (TyInterface n) = do
    cbs <- _cbs . _spec <$> get
    case M.lookup n cbs of
        Nothing -> throwE $ "Invalid callback name " ++ show n
        Just cb -> return cb
lookupCb ty = throwE $ "Invalid callback type " ++ show ty

lookupConsts :: Name -> Name -> ServeReq (Maybe Prim)
lookupConsts iname cname = do
    ifaces <- _ifaces . _spec <$> get
    case M.lookup iname ifaces of
        Just iface -> return $ M.lookup cname (_consts iface)
        Nothing -> throwE $ "Invalid Interface name: " ++ show iname

lookupCbMaybe :: IType -> ServeReq (Maybe Callback)
lookupCbMaybe (TyInterface n) = do
    cbs <- _cbs . _spec <$> get
    case M.lookup n cbs of
        Nothing -> return Nothing
        Just cb -> return $ Just cb
lookupCbMaybe _ = return Nothing

lookupClasss = do
    tn <- _traitsNew <$> get
    case tn of
        Just tn -> return tn
        Nothing -> _traits <$> get

lookupDefinition :: Name -> ServeReq (Maybe Definition)
lookupDefinition name = do
    spec <- _spec <$> get
    let f sel = M.lookup name (sel spec)
    case (f _ifaces, f _dicts) of
        (Just iface, Nothing) -> return $ Just (DefInterface iface)
        (Nothing, Just dict)  -> return $ Just (DefDictionary dict)
        _ -> return Nothing

-- Updates
updateMethod :: String -> String -> (ClassMemberMethod -> ClassMemberMethod) -> ServeReq ()
updateMethod tname fname f = do
    updateClass tname (\t ->
        let mtds = _tmethods t in
        case M.lookup fname mtds of
            Just mtd -> t { _tmethods = M.insert fname (f mtd) mtds }
            Nothing  -> t)

updateClass :: String -> (Class -> Class) -> ServeReq ()
updateClass x f = do
    traits <- lookupClasss
    case M.lookup x traits of
        Just t -> modify (\s -> s { _traitsNew = Just (M.insert x (f t) traits)})
        Nothing -> return ()

-- Add

addArg :: String -> DyType -> ServeReq ()
addArg x ty = let f = (\m -> m { _tlArgs = M.insert x ty (_tlArgs m) }) in modify (\s -> s { _tlm = f (_tlm s)} )

addRequire :: DyExpr -> ServeReq ()
addRequire e = let f = (\m -> m { _tlRequires = e : _tlRequires m }) in modify (\s -> s { _tlm = f (_tlm s)} )

addStmt :: Stmt -> ServeReq ()
addStmt s = let f = (\m -> m { _tlBody = _tlBody m ++ [s] }) in modify (\s -> s { _tlm = f (_tlm s)} ) 


addName :: Name -> String -> ServeReq ()
addName n x = modify (\s -> s { _names = M.insert n x (_names s)})

allocOnHeap :: Name -> ServeReq (JRef, String)
allocOnHeap iname = do
    h <- _heap <$> get
    let (r, h') = alloc (JsObj iname) h
    modify (\s -> s { _heap = h' })
    let vname = toLowerFirst (unName iname) ++ "_" ++ show (unJRef r)
    return (r, vname)

-- Namer

data Namer = Namer Int

initNamer :: Namer
initNamer = Namer 0

freshName :: Namer -> (String, Namer)
freshName (Namer x) = ("fresh_" ++ show x, Namer (x + 1))

fresh :: ServeReq String
fresh = do
    namer <- _namer <$> get
    let (x, namer') = freshName namer
    modify (\s -> s { _namer = namer' })
    return x

-- Misc

lvarToIfaceName :: LVar -> ServeReq Name
lvarToIfaceName = \case
    LInterface iname -> return iname
    LRef r -> do
        JsObj iname <- lookupObj r
        return iname
