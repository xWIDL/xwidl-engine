-- xWIDL specification AST
module Language.XWIDL.Spec where

import Language.JS.Type

import qualified Data.Map as M
import qualified Language.WebIDL.AST as W
import Prelude hiding (Enum)

data Spec = Spec {
    _ifaces     :: M.Map Name Interface,
    _dicts      :: M.Map Name Dictionary,
    _exceptions :: M.Map Name Exception,
    _enums      :: M.Map Name Enum,
    _cbs        :: M.Map Name Callback
} deriving Show

data Definition = DefInterface Interface
                | DefDictionary Dictionary
                | DefException Exception
                | DefEnum Enum
                | DefCallback Callback
                deriving Show

{-
    We will mimick Language.WebIDL.AST's naming, but
    customize AST in following aspects:

    - Combine the partial and typedef into other definitions
    - Instantiate inheritance
    - Ignore `implements` for now
    - Analyze the comments to get parsed annotations
    - Specialize the hanlding of `Constructor` extended attribute
    - Rewrite/Simplify the type structure
-}

data Interface = Interface {
    _iName :: Name,
    _iInherit :: Maybe Name,
    _constructors :: [InterfaceConstructor],
    _attrs :: M.Map Name Type,
    _ghostAttrs :: M.Map Name Type,
    _operations :: M.Map Name Operation
} deriving Show

data Dictionary = Dictionary {
  _dname :: Name,
  _dInherit :: Maybe Name,
  _dmembers :: [DictionaryMember]
} deriving Show

data Callback = Callback {
  _cName :: Name,
  _cRetTy :: Maybe Type,
  _cArgs :: [Argument]
} deriving Show

data DictionaryMember = DictionaryMember Type Name (Maybe W.Default) deriving Show

data Exception = Exception {
  _eName    :: Name,
  _eInherit :: Maybe Name,
  _emembers :: [ExceptionMember]
} deriving Show

data ExceptionMember = ExConst Const
                     | ExField Type Name
                     deriving Show

data Const = Const Type Name W.ConstValue deriving Show

data Enum = Enum Name [String] deriving Show

data Operation = Operation {
    _imName :: Name,
    _imArgs :: [Argument],
    _imRet  :: Maybe Type,
    _imEnsures :: Maybe String,
    _imRequires :: Maybe String,
    _imCbs :: [CallbackSpec]
    -- _imEffects :: Maybe String -- TODO
} deriving Show

data CallbackSpec = CallbackSpec Name String [String] deriving Show

data Argument = Argument Name Type (Maybe W.Default) deriving Show

data InterfaceConstructor = InterfaceConstructor {
    _icArgs :: [Argument],
    _icEnsures :: Maybe String,
    _icRequires :: Maybe String
} deriving Show

data Type = TyInterface Name
          | TyDOMString
          | TyNullable Type
          | TyInt | TyFloat | TyAny
          | TyBoolean | TyObject
          | TyBuiltIn Name
          | TyArray Type
          | TyUnion [Type]
          deriving (Show)
