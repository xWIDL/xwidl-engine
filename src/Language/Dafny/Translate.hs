-- Translation from xIWDL spec to Dafny traits
{-# LANGUAGE TupleSections #-}

module Language.Dafny.Translate (translateSpec) where

import Language.XWIDL.Spec
import Language.Dafny.AST
import Language.JS.Type

import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.Except

-- Interface translation
type Trans = ReaderT Spec (Except String)

translateSpec :: Spec -> Either String [Trait]
translateSpec s@(Spec ifaces _ _ _ _) =
    runExcept (runReaderT (mapM translateIface (M.elems ifaces)) s)

translateIface :: Interface -> Trans Trait
translateIface (Interface iname mInherit constructors _attrs gAttrs operations) =
    case mInherit of
        Just parent -> do
            ifaces <- _ifaces <$> ask
            case M.lookup parent ifaces of
                Just piface -> do -- TODO: optimization
                    Trait _ pattrs pmethods <- translateIface piface
                    return $ Trait (unName iname) (pattrs ++ attrs) (pmethods ++ methods)
                Nothing -> throwError $ "Invalid inheritance: " ++ show parent
        Nothing ->
            return $ Trait (unName iname) attrs methods
    where
        attrs = map (\(x, ty) -> (unName x, iTypeToDyType ty)) (M.toList gAttrs)
        methods = map (translateConstructor iname) (zip [0..] constructors) ++
                  map translateMethod (M.elems operations)

translateConstructor :: Name -> (Int, InterfaceConstructor) -> TraitMemberMethod
translateConstructor iname (idx, InterfaceConstructor{..}) = tmm
    where tmm = TraitMemberMethod {
        _tmName = "new_" ++ show idx,
        _tmArgs = map (\(Argument x ity _) -> (unName x, iTypeToDyType ity)) _icArgs,
        _tmRet  = Just ("ret", DTyClass (unName iname)),
        _tmEnsures = _icEnsures,
        _tmRequires = _icRequires
        -- _tmImpl = Nothing
    }

translateMethod :: Operation -> TraitMemberMethod
translateMethod Operation{..} = tmm
    where tmm = TraitMemberMethod {
        _tmName = unName _imName,
        _tmArgs = map (\(Argument x ity _) -> (unName x, iTypeToDyType ity)) _imArgs,
        _tmRet  = (("ret",) . iTypeToDyType) <$> _imRet,
        _tmEnsures = _imEnsures,
        _tmRequires = _imRequires
        -- _tmImpl = _imEffects
    }

iTypeToDyType :: Type -> DyType
iTypeToDyType = \case
    TyInterface x -> DTyClass (unName x)
    TyDOMString   -> DTyString
    TyNullable (TyInterface x) -> DTyClass (unName x)
    TyInt -> DTyInt
    TyFloat -> DTyReal
    ty -> error $ "Can't translate Type: " ++ show ty
