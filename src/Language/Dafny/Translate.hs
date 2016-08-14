-- Translation from xIWDL spec to Dafny traits
{-# LANGUAGE TupleSections #-}

module Language.Dafny.Translate (
    translateSpec, getUnionIfaceName, iTypeToDyType
) where

import Language.XWIDL.Spec
import Language.Dafny.AST
import Language.JS.Type
import Data.String.Utils

import qualified Data.Map as M

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

translateSpec :: Spec -> Either String (M.Map String Trait, Datatypes)
translateSpec s@(Spec ifaces dicts _ _) =
    fmap (\(traits, s') -> (traits, _datatypes s'))
         (runExcept (runStateT m (TransState { _spec = s, _datatypes = M.empty })))
    where
        m = do
                ifaceTraits <- M.fromList <$> mapM translateIface (M.elems ifaces)
                dictTraits <- M.fromList <$> mapM translateDict (M.elems dicts)
                return (ifaceTraits `M.union` dictTraits)

translateIface :: Interface -> Trans (String, Trait)
translateIface (Interface iname mInherit constructors _consts _attrs gAttrs operations) = do
    contrs <- case constructors of
                InterfaceConstructors conss -> M.fromList <$> mapM (translateConstructor iname) (zip [0..] conss)
                InterfaceHTMLConstructor -> return $ M.singleton "new_def" defConstructor
    tms <- mapM translateMethod (M.elems operations)
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
    let bindAttrsStr = join " && " (map (\k -> "ret." ++ k ++ " == " ++ k) (M.keys attrs))
    -- constructor
    let cons = TraitMemberMethod {
                    _tmName = "new_def",
                    _tmArgs = M.elems attrs,
                    _tmRet  = Just ("ret", DTyClass (unName dname)),
                    _tmEnsures = Just ("ret != null && " ++ bindAttrsStr),
                    _tmRequires = Nothing
                }
    let methods = M.singleton "new_def" cons
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

translateMethod :: Operation -> Trans TraitMemberMethod
translateMethod Operation{..} = do
    cbs <- _cbs . _spec <$> get
    
    args <- mapM (\(Argument x ity _) -> (unName x,) <$> iType_ToDyType ity) $ map (replaceCb cbs) _imArgs
    optargs <- mapM (\(Argument x ity _) -> (unName x,) . DTyOpt <$> iType_ToDyType ity) $ map (replaceCb cbs) _imOptArgs

    let args' = args ++ optargs
    let retty = fmap (\ty -> ("ret", iTypeToDyType ty)) _imRet
    
    return TraitMemberMethod {
        _tmName = unName _imName,
        _tmArgs = args',
        _tmRet  = retty,
        _tmEnsures = _imEnsures,
        _tmRequires = _imRequires
    }

replaceCb :: M.Map Name Callback -> Argument -> Argument
replaceCb cbs arg = do
    case _argTy arg of
        (ITySingle (TyInterface n)) -> do
            case M.lookup n cbs of
                Nothing -> arg
                Just _  -> Argument (_argName arg) (ITySingle (TyInterface $ Name "CallbackTrait")) (_argOptDef arg)
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
