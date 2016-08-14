module Type where

import Language.JS.Type
import Language.XWIDL.Spec
import Language.Dafny.AST

pTyToIType :: PrimType -> IType
pTyToIType = \case
    PTyNull -> error "can't infer type for null value"
    PTyNumber -> TyFloat
    PTyInt -> TyInt
    PTyString -> TyDOMString
    PTyBool -> TyBoolean

dyTypeToIType :: DyType -> IType
dyTypeToIType = \case
    DTyClass x -> TyInterface (Name x)
    DTyString -> TyDOMString
    DTyInt -> TyInt
    DTyBool -> TyBoolean
    DTyReal -> TyFloat
    DTyOpt _ -> error "can't process optional type"
    DTyADT _ -> error "TyUnion"

pTyToDTy :: PrimType -> DyType
pTyToDTy PTyNull = error "can't process null type"
pTyToDTy PTyNumber = DTyReal
pTyToDTy PTyInt = DTyInt
pTyToDTy PTyString = DTyString
pTyToDTy PTyBool = DTyBool
