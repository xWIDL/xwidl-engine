-- Translation from xIWDL spec to Dafny classs
{-# LANGUAGE TupleSections #-}

module Language.Dafny.Translate (
    translateSpec, getUnionIfaceName, iTypeToDyType,
    topSortClasses
) where

import Language.XWIDL.Spec
import Language.Dafny.AST
import Language.JS.Type
import Data.String.Utils

import Data.Graph
import Control.Arrow

import qualified Data.Map as M
import qualified Data.Set as S

import Util

import Control.Monad (filterM)
import Control.Monad.State hiding (join)
import Control.Monad.Except hiding (join)

-- Interface translation
type Trans = StateT TransState (Except String)

type Datatypes = M.Map String [(String, DyType)]

data TransState = TransState {
    _spec :: Spec,
    _datatypes :: Datatypes
}

translateSpec :: Spec -> Either String (M.Map String Class, Datatypes)
translateSpec s@(Spec ifaces dicts _ _) =
    fmap (\(classes, s') -> (classes, _datatypes s'))
         (runExcept (runStateT m (TransState { _spec = s, _datatypes = M.empty })))
    where
        m = do
                ifaceClasses <- map (\c -> (_tname c, c)) <$> mapM translateIface (M.elems ifaces)
                dictClasses <- map (\c -> (_tname c, c)) <$> mapM translateDict (M.elems dicts)
                return $ M.fromList $ ifaceClasses ++ dictClasses

topSortClasses :: [Class] -> [Class]
topSortClasses classes =
    let edges = map (\c -> (c, _tname c, S.toList $ _tdeps c)) classes
        (g, fv, fk) = graphFromEdges edges
        nodes = map fv (topSort g)
    in  map (\(c, _, _) -> c) nodes

translateIface :: Interface -> Trans Class
translateIface (Interface iname mInherit constructors _consts attrs gAttrs operations) = do
    (constrs, deps1) <- case constructors of
                           InterfaceConstructors conss ->
                               second S.unions . unzip <$> mapM translateConstructor conss
                           InterfaceHTMLConstructor -> return ([defConstructor], S.empty)
    (tms, deps2) <- second S.unions . unzip <$> mapM translateMethod (M.elems operations)
    let deps = deps1 `S.union` deps2
    let tmsMap = M.fromList $ map (\t -> (_tmName t, t)) tms
    let setters = M.fromList $ map (\(x, ty) -> let fname = "set_" ++ unName x
                                                in  (fname, defSetter fname))
                                   (M.toList attrs)
    let methods = tmsMap `M.union` setters
    let allAttrs = M.fromList $ map (\(x, ty) -> (unName x, (unName x, iTypeToDyType ty)))
                                 (M.toList gAttrs ++ M.toList attrs)
    let tname = unName iname
    case mInherit of
        Just parent -> do
            ifaces <- _ifaces . _spec <$> get
            case M.lookup parent ifaces of
                Just piface -> do -- TODO: optimization
                    Class _ deps' pattrs pcons pmethods <- translateIface piface
                    return $ Class tname (deps `S.union` deps') (pattrs `M.union` allAttrs)
                                   (pcons ++ constrs) (pmethods `M.union` methods)
                Nothing -> throwError $ "Invalid inheritance: " ++ show parent
        Nothing ->
            return $ Class tname deps allAttrs constrs methods
    where
        defConstructor = ClassConstructor {
            _tcArgs = [],
            _tcRequires = Nothing
        }

        defSetter fname = ClassMemberMethod {
            _tmName = fname,
            _tmArgs = [],
            _tmRet  = Nothing,
            _tmEnsures = Nothing,
            _tmRequires = Nothing
        }

translateDict :: Dictionary -> Trans Class
translateDict (Dictionary dname mInherit dmembers) = do
    let attrs = M.fromList $ map (\(DictionaryMember ty x _) ->
                                        (unName x, (unName x, iTypeToDyType ty)))
                                 dmembers
    let deps = filterDeps $ map (\(DictionaryMember ty _ _) -> iTypeToDyType ty) dmembers
    let tname = unName dname
    -- let bindAttrsStr = join " && " (map (\k -> "ret." ++ k ++ " == " ++ k) (M.keys attrs))
    -- XXX: fix dictionary binding
    -- constructor
    let cons = ClassConstructor {
                    _tcArgs = [],
                    _tcRequires = Nothing
                }
    case mInherit of
        Just parent -> do
            dicts <- _dicts . _spec <$> get
            case M.lookup parent dicts of
                Just pdict -> do -- TODO: optimization
                    Class _ deps' pattrs pcons pmethods <- translateDict pdict
                    return $ Class tname (deps `S.union` deps') (pattrs `M.union` attrs) (cons:pcons) pmethods
                Nothing -> throwError $ "Invalid inheritance: " ++ show parent
        Nothing ->
            return $ Class tname deps attrs [cons] M.empty

translateConstructor :: InterfaceConstructor -> Trans (ClassConstructor, Deps)
translateConstructor InterfaceConstructor{..} = do
    args <- mapM (\(Argument x ity _) -> (unName x,) <$> iType_ToDyType ity) _icArgs
    optargs <- mapM (\(Argument x ity _) -> (unName x,) . DTyOpt <$> iType_ToDyType ity) _icOptArgs
    let argsAll = args ++ optargs
    let deps = filterDeps $ map snd argsAll
    return (ClassConstructor {
            _tcArgs = argsAll,
            _tcRequires = _icRequires
    }, deps)

translateMethod :: Operation -> Trans (ClassMemberMethod, Deps)
translateMethod Operation{..} = do
    cbs <- _cbs . _spec <$> get
    
    args <- mapM (\(Argument x ity _) -> (unName x,) <$> iType_ToDyType ity) $ map (replaceCb cbs) _imArgs
    optargs <- mapM (\(Argument x ity _) -> (unName x,) . DTyOpt <$> iType_ToDyType ity) $ map (replaceCb cbs) _imOptArgs

    let argsAll = args ++ optargs
    let retty = fmap (\ty -> ("ret", iTypeToDyType ty)) _imRet

    let deps = filterDeps $ map snd argsAll
    
    return (ClassMemberMethod {
        _tmName = unName _imName,
        _tmArgs = argsAll,
        _tmRet  = retty,
        _tmEnsures = _imEnsures,
        _tmRequires = _imRequires
    }, deps)

replaceCb :: M.Map Name Callback -> Argument -> Argument
replaceCb cbs arg = do
    case _argTy arg of
        (ITySingle (TyInterface n)) -> do
            case M.lookup n cbs of
                Nothing -> arg
                Just _  -> Argument (_argName arg) (ITySingle (TyInterface $ Name "CallbackClass")) (_argOptDef arg)
        _ -> arg

iType_ToDyType :: IType_ -> Trans DyType
iType_ToDyType = \case
    ITyUnion tys  -> makeDatatype tys
    ITySingle ty -> return $ iTypeToDyType ty

iTypeToDyType :: IType -> DyType
iTypeToDyType = \case
    TyInterface x -> DTyClass (unName x)
    TyDOMString   -> DTyString
    TyNullable (TyInterface x) -> DTyClass (unName x)
    TyInt -> DTyInt
    TyFloat -> DTyReal
    TyBoolean -> DTyBool
    ty -> error $ "Can't translate Type: " ++ show ty

makeDatatype :: [IType] -> Trans DyType
makeDatatype tys = do
    let tyName = getUnionIfaceName tys -- XXX: use prettyShow
    let tys' = map iTypeToDyType tys
    ret <- lookupDatatype tyName
    case ret of
        Nothing -> registerDatatype tyName (map (\ty -> (prettyShow ty, ty)) tys')
        Just _ -> return (DTyADT tyName)

registerDatatype :: String -> [(String, DyType)] -> Trans DyType
registerDatatype tyName constrs = do
    modify (\s -> s { _datatypes = M.insert tyName constrs (_datatypes s)})
    return $ DTyADT tyName

getUnionIfaceName :: [IType] -> String
getUnionIfaceName itys = join "Or" (map prettyShow itys)

lookupDatatype :: String -> Trans (Maybe [(String, DyType)])
lookupDatatype x = M.lookup x <$> _datatypes <$> get

filterDeps :: [DyType] -> Deps
filterDeps = S.fromList .
             map (\(DTyClass s) -> s) .
             filter (\case
                        DTyClass _ -> True
                        _ -> False)


