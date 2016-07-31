import Session
import Spec

import qualified Data.Map as M
import JS.Type
import WebIDL
import Language.WebIDL.Parser
import qualified Language.WebIDL.AST as W
import Translate

import qualified Data.Map as M

main = do
    result <- parseIDL <$> readFile "examples/ex-1/spec.idl"
    case result of
        Right defs ->
            case transDefsToSpec defs of
                Right spec -> run spec
                Left err -> putStrLn err
        Left err -> print err
