-- Translate WebIDL into xWIDL
module WebIDL where

import qualified Language.WebIDL.AST as W
import qualified Spec as S
import Language.WebIDL.Parser (Tag)

import Control.Monad.State
import Control.Monad.Except

-- Interface translation
type ITrans = ExceptT String (State S.Interface)

transIface :: W.Interface Tag -> ITrans S.Interface
transIface = undefined
