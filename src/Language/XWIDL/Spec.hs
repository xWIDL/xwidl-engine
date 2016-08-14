-- xWIDL specification AST
module Language.XWIDL.Spec where

import Language.JS.Type

import qualified Data.Map as M
import qualified Language.WebIDL.AST as W

import Text.PrettyPrint.Leijen

data Spec = Spec {
    _ifaces     :: M.Map Name Interface,
    _dicts      :: M.Map Name Dictionary,
    _exceptions :: M.Map Name Exception,
    _cbs        :: M.Map Name Callback
} deriving (Eq, Show)

data Definition = DefInterface Interface
                | DefDictionary Dictionary
                | DefException Exception
                | DefCallback Callback
                deriving (Eq, Show)

data InterfaceConstructors = InterfaceConstructors [InterfaceConstructor]
                           | InterfaceHTMLConstructor
                           deriving (Eq, Show)

data Interface = Interface {
    _iName :: Name,
    _iInherit :: Maybe Name,
    _constructors :: InterfaceConstructors,
    _consts :: M.Map Name Prim,
    _attrs :: M.Map Name IType,
    _ghostAttrs :: M.Map Name IType,
    _operations :: M.Map Name Operation
} deriving (Eq, Show)

data Dictionary = Dictionary {
  _dname :: Name,
  _dInherit :: Maybe Name,
  _dmembers :: [DictionaryMember]
} deriving (Eq, Show)

data Callback = Callback {
  _cName :: Name,
  _cRetTy :: Maybe IType,
  _cArgs :: [Argument]
} deriving (Show, Eq)

data DictionaryMember = DictionaryMember IType Name (Maybe W.Default) deriving (Eq, Show)

data Exception = Exception {
  _eName    :: Name,
  _eInherit :: Maybe Name,
  _emembers :: [ExceptionMember]
} deriving (Eq, Show)

data ExceptionMember = ExConst Const
                     | ExField IType Name
                     deriving (Eq, Show)

data Const = Const IType Name W.ConstValue deriving (Eq, Show)

data Operation = Operation {
    _imName :: Name,
    _imArgs :: [Argument],
    _imOptArgs :: [Argument],
    _imRet  :: Maybe IType,
    _imEnsures :: Maybe String,
    _imRequires :: Maybe String,
    _imCbs :: [CallbackSpec]
} deriving (Eq, Show)

data CallbackSpec = CallbackSpec Name String [String] deriving (Eq, Show)

data Argument = Argument {
  _argName   :: Name,
  _argTy     :: IType_,
  _argOptDef :: Maybe W.Default
} deriving (Eq, Show)

data InterfaceConstructor = InterfaceConstructor {
    _icArgs :: [Argument],
    _icOptArgs :: [Argument],
    _icRequires :: Maybe String
} deriving (Eq, Show)

-- optional type? ... yes, now it is a bit weird
data IType = TyInterface Name
           | TyNullable IType
           | TyBuiltIn Name
           | TyAny | TyObject
           | TyBoolean | TyInt | TyFloat | TyDOMString
           | TyArray IType
           deriving (Eq, Show)

data IType_ = ITyUnion [IType]
            | ITySingle IType
            deriving (Eq, Show)

instance Pretty IType where
  pretty = \case
    TyInterface (Name x) -> text x
    TyNullable ty -> pretty ty
    TyBuiltIn (Name x) -> text x
    TyAny -> text "Any"
    TyObject -> text "Object"
    TyBoolean -> text "Boolean"
    TyInt -> text "Int"
    TyFloat -> text "Float"
    TyDOMString -> text "DOMString"
    TyArray _ -> error "doesn't support TyArray yet"
