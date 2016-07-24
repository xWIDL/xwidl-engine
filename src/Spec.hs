-- xWIDL specification AST
module Spec where

import JS.Type
import qualified Data.Map as M
import qualified Language.WebIDL.AST as W
import Prelude hiding (Enum)

data Spec = Spec {
    _defs :: M.Map Name Definition
}

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

data Definition = DefInterface Interface
                | DefDictionary Dictionary
                | DefException Exception
                | DefEnum Enum

data Interface = Interface {
    _iName :: Name,
    _constructors :: [InterfaceConstructor],
    _attrs :: M.Map Name Type,
    _ghostAttrs :: M.Map Name Type,
    _operations :: M.Map Name Operation
}

data Dictionary = Dictionary Name [DictionaryMember]

data DictionaryMember = DictionaryMember Type Name (Maybe W.Default)

data Exception = Exception Name [ExceptionMember]

data ExceptionMember = ExConst Const
                     | ExField Type Name

data Const = Const Type Name W.ConstValue

data Enum = Enum Name [String]

data Operation = Operation {
    _imName :: Name,
    _imArgs :: [Argument],
    _imRet  :: Maybe Type,
    _imEnsures :: Maybe String,
    _imRequires :: Maybe String
    -- _imEffects :: Maybe String -- TODO
}

data Argument = Argument Name Type (Maybe W.Default)

data InterfaceConstructor = InterfaceConstructor {
    _icArgs :: [Argument],
    _icEnsures :: Maybe String,
    _icRequires :: Maybe String
}

data Type = TyInterface Name
          | TyDOMString
          | TyNullable Type
          | TyInt | TyFloat | TyAny
          | TyBoolean | TyObject
          | TyBuiltIn Name
          | TyArray Type
          | TyUnion [Type]
          deriving (Show)
