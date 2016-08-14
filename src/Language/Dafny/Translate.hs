-- Translation from xIWDL spec to Dafny traits
{-# LANGUAGE TupleSections #-}

module Language.Dafny.Translate (
    translateSpec, getUnionIfaceName,
    iTypeToDyType
) where

import Language.XWIDL.Spec
import Language.Dafny.AST
import Language.JS.Type
import Data.String.Utils

import qualified Data.Map as M

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

translateSpec :: Spec -> Either String (M.Map String Trait, Datatypes)
translateSpec s@(Spec ifaces dicts _ _) =
    fmap (\(traits, s) -> (traits, _datatypes s))
         (runExcept (runStateT m (TransState { _spec = s, _datatypes = M.empty })))
    where
        m = do
                ifaceTraits <- M.fromList <$> mapM translateIface (M.elems ifaces)
                dictTraits <- M.fromList <$> mapM translateDict (M.elems dicts)
                return (ifaceTraits `M.union` dictTraits)

translateIface :: Interface -> Trans (String, Trait)
translateIface (Interface iname mInherit constructors _attrs gAttrs operations) = do
    contrs <- case constructors of
                InterfaceConstructors conss -> M.fromList <$> mapM (translateConstructor iname) (zip [0..] conss)
                InterfaceHTMLConstructor -> return $ M.singleton "new_def" defConstructor
    tms <- concat <$> mapM translateMethod (M.elems operations)
    let tmsMap = M.fromList $ map (\t -> (_tmName t, t)) tms
    let methods = contrs `M.union` tmsMap
    let attrs = M.fromList $ map (\(x, ty) -> (unName x, (unName x, iTypeToDyType ty)))
                                 (M.toList gAttrs)
    let tname = unName iname
    case mInherit of
        Just parent -> do
            ifaces <- _ifaces . _spec <$> get
            case M.lookup parent ifaces of
                Just piface -> do -- TODO: optimization
                    Trait _ pattrs pmethods <- snd <$> translateIface piface
                    return (tname, Trait tname (pattrs `M.union` attrs) (pmethods `M.union` methods))
                Nothing -> throwError $ "Invalid inheritance: " ++ show parent
        Nothing ->
            return (tname, Trait tname attrs methods)
    where
        defConstructor = TraitMemberMethod {
            _tmName = "new_def",
            _tmArgs = [],
            _tmRet = Just ("ret", DTyClass (unName iname)),
            _tmEnsures = Nothing, -- XXX: maybe ret != null?
            _tmRequires = Nothing
        }

translateDict :: Dictionary -> Trans (String, Trait)
translateDict (Dictionary dname mInherit dmembers) = do
    let attrs = M.fromList $ map (\(DictionaryMember ty x _) ->
                                        (unName x, (unName x, iTypeToDyType ty)))
                                 dmembers

    let tname = unName dname
    -- constructor
    let cons = TraitMemberMethod {
                    _tmName = "new",
                    _tmArgs = M.elems attrs,
                    _tmRet  = Just ("ret", DTyClass (unName dname)),
                    _tmEnsures = Nothing,
                    _tmRequires = Nothing
                }
    let methods = M.singleton "new" cons
    case mInherit of
        Just parent -> do
            dicts <- _dicts . _spec <$> get
            case M.lookup parent dicts of
                Just pdict -> do -- TODO: optimization
                    Trait _ pattrs pmethods <- snd <$> translateDict pdict
                    return (tname, Trait tname (pattrs `M.union` attrs) pmethods)
                Nothing -> throwError $ "Invalid inheritance: " ++ show parent
        Nothing ->
            return (tname, Trait tname attrs methods)

translateConstructor :: Name -> (Int, InterfaceConstructor) -> Trans (String, TraitMemberMethod)
translateConstructor iname (idx, InterfaceConstructor{..}) = do
    args <- mapM (\(Argument x ity _) -> (unName x,) <$> iType_ToDyType ity) _icArgs    
    let tmName = "new_" ++ show idx
    let tmm = TraitMemberMethod {
            _tmName = tmName,
            _tmArgs = args,
            _tmRet  = Just ("ret", DTyClass (unName iname)),
            _tmEnsures = _icEnsures,
            _tmRequires = _icRequires
    }
    return (tmName, tmm)

translateMethod :: Operation -> Trans [TraitMemberMethod]
translateMethod Operation{..} = do
    args <- filterM (notCb . _argTy) _imArgs
    optargs <- filterM (notCb . _argTy) _imOptArgs
    let argss = mapArg iTypeToDyType args
    let optargss = mapArg (DTyOpt . iTypeToDyType) optargs
    let argss = argss ++ optargss
    let retty = fmap (\ty -> ("eret", iTypeToDyType ty)) _imRet
    forM (merge argss) $ \args'' -> do
        return TraitMemberMethod {
            _tmName = unName _imName,
            _tmArgs = zip (fst args'') (snd args''),
            _tmRet  = retty,
            _tmEnsures = _imEnsures,
            _tmRequires = _imRequires
        }

mapArg :: (IType -> DyType) -> [Argument] -> [[(String, DyType)]]
mapArg f args = map (\(Argument name ty _) ->
                        case ty of
                            ITySingle ty' -> [(unName name, f ty')]
                            ITyUnion tys  -> map (\ty' -> (unName name, f ty')) tys)
                    args

merge :: [[(String, DyType)]] -> [([String], [DyType])]
merge [] = []
merge [choices] = map (\(val, ty) -> ([val], [ty])) choices
merge (choices:args) = concatMap (\(val, ty) -> map (\(vals, tys) -> (val : vals, ty: tys)) (merge args)) choices

notCb :: IType_ -> Trans Bool
notCb (ITySingle (TyInterface n)) = do
    cbs <- _cbs . _spec <$> get
    case M.lookup n cbs of
        Nothing -> return True
        Just _  -> return False
notCb _ = return True

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
    ty -> error $ "Can't translate Type: " ++ show ty

makeDatatype :: [IType] -> Trans DyType
makeDatatype tys = do
    let tyName = getUnionIfaceName tys
    let tys' = map iTypeToDyType tys
    ret <- lookupDatatype tyName
    case ret of
        Nothing -> registerDatatype tyName (map (\ty -> (tyName ++ "_as_" ++ show ty, ty)) tys')
        Just _ -> return (DTyADT tyName)

registerDatatype :: String -> [(String, DyType)] -> Trans DyType
registerDatatype tyName constrs = do
    modify (\s -> s { _datatypes = M.insert tyName constrs (_datatypes s)})
    return $ DTyADT tyName

getUnionIfaceName :: [IType] -> String
getUnionIfaceName itys = join "Or" (map flatten itys)
    where
        flatten = \case
            TyInterface (Name x) -> x
            TyNullable ty -> flatten ty
            TyBuiltIn (Name x) -> x
            TyAny -> "Any"
            TyObject -> "Object"
            TyBoolean -> "Boolean"
            TyInt -> "Int"
            TyFloat -> "Float"
            TyDOMString -> "DOMString"
            TyArray _ -> error "doesn't support array type yet"

lookupDatatype :: String -> Trans (Maybe [(String, DyType)])
lookupDatatype x = M.lookup x <$> _datatypes <$> get
