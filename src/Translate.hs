-- Translation from xIWDL spec to Dafny traits
{-# LANGUAGE TupleSections #-}

module Translate where

import Spec
import Dafny
import JS.Type

import qualified Data.Map as M

translateSpec :: Spec -> [Trait]
translateSpec (Spec ifaces) = flip map (M.elems ifaces) $ \case
    DefInterface i -> translateIface i
    other -> error $ "can't translate " ++ show other

translateIface :: Interface -> Trait
translateIface (Interface iname constructors _attrs gAttrs methods) =
    Trait (unName iname)
          (map (\(x, ty) -> (unName x, iTypeToDyType ty)) (M.toList gAttrs))
          (map (translateConstructor iname) (zip [0..] constructors)
           ++ map translateMethod (M.elems methods))

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
    ty -> error $ "Can't translate Type: " ++ show ty
