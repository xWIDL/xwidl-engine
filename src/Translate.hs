-- Translation from xIWDL spec to Dafny traits
{-# LANGUAGE TupleSections #-}

module Translate where

import Spec
import Dafny
import JS.Type
import Model

import qualified Data.Map as M

translateSpec :: Spec -> [Trait]
translateSpec (Spec ifaces) = map translateIface (M.elems ifaces)

translateIface :: Interface -> Trait
translateIface (Interface iname constructors ghostStates methods) =
    Trait (unName iname)
          (map (\(x, ty) -> (unName x, iTypeToDyType ty)) (M.toList ghostStates))
          (map (translateConstructor iname) (zip [0..] constructors)
           ++ map translateMethod (M.elems methods))

translateConstructor :: Name -> (Int, InterfaceConstructor) -> TraitMemberMethod
translateConstructor iname (idx, InterfaceConstructor{..}) = tmm
    where tmm = TraitMemberMethod {
        _tmName = "new_" ++ show idx,
        _tmArgs = map (\(x, ity) -> (unName x, iTypeToDyType ity)) _icArgs,
        _tmRet  = Just ("ret", DTyClass (unName iname)),
        _tmEnsures = _icEnsures,
        _tmRequires = _icRequires
        -- _tmImpl = Nothing
    }

translateMethod :: InterfaceMethod -> TraitMemberMethod
translateMethod InterfaceMethod{..} = tmm
    where tmm = TraitMemberMethod {
        _tmName = unName _imName,
        _tmArgs = map (\(x, ity) -> (unName x, iTypeToDyType ity)) _imArgs,
        _tmRet  = (("ret",) . iTypeToDyType) <$> _imRet,
        _tmEnsures = _imEnsures,
        _tmRequires = _imRequires
        -- _tmImpl = _imEffects
    }

iTypeToDyType :: IType -> DyType
iTypeToDyType = \case
    ITyInterface x -> DTyClass (unName x)
    ITyDOMString   -> DTyString
    ITyNullable (ITyInterface x) -> DTyClass (unName x)
    ITyInt -> DTyInt
    ty -> error $ "Can't translate IType: " ++ show ty
