import Session
import Translate

import JS.Type

import Language.XWIDL.Spec
import Language.XWIDL.WebIDL

import Language.WebIDL.Parser
import qualified Language.WebIDL.AST as W

import qualified Data.Map as M

main = do
    result <- parseIDL <$> readFile "examples/ex-1/spec.idl"
    case result of
        Right defs ->
            case transDefsToSpec defs of
                Right spec -> run spec
                Left err -> putStrLn err
        Left err -> print err
