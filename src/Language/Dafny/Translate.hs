-- Translation from xIWDL spec to Dafny traits
{-# LANGUAGE TupleSections #-}

module Language.Dafny.Translate (translateSpec) where

import Language.XWIDL.Spec
import Language.Dafny.AST
import Language.JS.Type

import qualified Data.Map as M

import Control.Monad (filterM)
import Control.Monad.Reader
import Control.Monad.Except

-- Interface translation
type Trans = ReaderT Spec (Except String)

translateSpec :: Spec -> Either String (M.Map String Trait)
translateSpec s@(Spec ifaces dicts _ _) =
    runExcept (runReaderT (do
                                ifaceTraits <- M.fromList <$> mapM translateIface (M.elems ifaces)
                                dictTraits <- M.fromList <$> mapM translateDict (M.elems dicts)
                                return (ifaceTraits `M.union` dictTraits)
                                ) s)

translateIface :: Interface -> Trans (String, Trait)
translateIface (Interface iname mInherit constructors _attrs gAttrs operations) = do
    let contrs = M.fromList $ map (translateConstructor iname) (zip [0..] constructors)
    tms <- mapM translateMethod $ M.elems operations
    let tmsMap = M.fromList $ map (\t -> (_tmName t, t)) tms
    let methods = contrs `M.union` tmsMap
    let attrs = M.fromList (map (\(x, ty) -> (unName x, (unName x, iTypeToDyType ty))) $ M.toList gAttrs)
    let tname = unName iname
    case mInherit of
        Just parent -> do
            ifaces <- _ifaces <$> ask
            case M.lookup parent ifaces of
                Just piface -> do -- TODO: optimization
                    Trait _ pattrs pmethods <- snd <$> translateIface piface
                    return (tname, Trait tname (pattrs `M.union` attrs) (pmethods `M.union` methods))
                Nothing -> throwError $ "Invalid inheritance: " ++ show parent
        Nothing ->
            return (tname, Trait tname attrs methods)

translateDict :: Dictionary -> Trans (String, Trait)
translateDict (Dictionary dname mInherit dmembers) = do
    let attrs = M.fromList (map (\(DictionaryMember ty x _) ->
                                  (unName x, (unName x, iTypeToDyType ty))) dmembers)
    let tname = unName dname
    -- constructor
    let cons = TraitMemberMethod {
                    _tmName = "new",
                    _tmArgs = [],
                    _tmRet  = Just ("ret", DTyClass (unName dname)),
                    _tmEnsures = Nothing,
                    _tmRequires = Nothing
                    -- _tmImpl = Nothing
                }
    let methods = M.singleton "new" cons
    case mInherit of
        Just parent -> do
            dicts <- _dicts <$> ask
            case M.lookup parent dicts of
                Just pdict -> do -- TODO: optimization
                    Trait _ pattrs pmethods <- snd <$> translateDict pdict
                    return (tname, Trait tname (pattrs `M.union` attrs) pmethods)
                Nothing -> throwError $ "Invalid inheritance: " ++ show parent
        Nothing ->
            return (tname, Trait tname attrs methods)

translateConstructor :: Name -> (Int, InterfaceConstructor) -> (String, TraitMemberMethod)
translateConstructor iname (idx, InterfaceConstructor{..}) = (tmName, tmm)
    where
        tmName = "new_" ++ show idx
        tmm = TraitMemberMethod {
            _tmName = tmName,
            _tmArgs = map (\(Argument x ity _) -> (unName x, iTypeToDyType ity)) _icArgs,
            _tmRet  = Just ("ret", DTyClass (unName iname)),
            _tmEnsures = _icEnsures,
            _tmRequires = _icRequires
            -- _tmImpl = Nothing
        }

translateMethod :: Operation -> Trans TraitMemberMethod
translateMethod Operation{..} = do
    args <- filterM notCb _imArgs
    return TraitMemberMethod {
        _tmName = unName _imName,
        _tmArgs = map (\(Argument x ity _) -> (unName x, iTypeToDyType ity)) args,
        _tmRet  = (("ret",) . iTypeToDyType) <$> _imRet,
        _tmEnsures = _imEnsures,
        _tmRequires = _imRequires
        -- _tmImpl = _imEffects
    }

notCb :: Argument -> Trans Bool
notCb (Argument _ (TyInterface n) _) = do
    cbs <- _cbs <$> ask
    case M.lookup n cbs of
        Nothing -> return True
        Just _  -> return False
notCb _ = return True

iTypeToDyType :: Type -> DyType
iTypeToDyType = \case
    TyInterface x -> DTyClass (unName x)
    TyDOMString   -> DTyString
    TyNullable (TyInterface x) -> DTyClass (unName x)
    TyInt -> DTyInt
    TyFloat -> DTyReal
    ty -> error $ "Can't translate Type: " ++ show ty
