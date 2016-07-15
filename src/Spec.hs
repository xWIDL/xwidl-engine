-- xWIDL specification AST
module Spec where

import JS.Type
import qualified Data.Map as M

data Spec = Spec {
    _interfaces :: M.Map Name Interface
}

data Interface = Interface {
    _iName :: Name,
    _constructors :: [InterfaceConstructor],
    _ghostStates :: M.Map Name IType,
    _methods :: M.Map Name InterfaceMethod
}

data InterfaceMethod = InterfaceMethod {
    _imName :: Name,
    _imArgs :: [(Name, IType)],
    _imRet  :: Maybe IType,
    _imEnsures :: Maybe String,
    _imRequires :: Maybe String
    -- _imEffects :: Maybe String
}

data InterfaceConstructor = InterfaceConstructor {
    _icArgs :: [(Name, IType)],
    _icEnsures :: Maybe String,
    _icRequires :: Maybe String
}

data IType = ITyInterface Name
           | ITyDOMString
           | ITyNullable IType
           | ITyInt
           deriving (Show)
