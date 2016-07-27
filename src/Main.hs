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
        Right defs -> do
            let ifaces = map (\(W.DefInterface i) -> i) defs -- FIXME
            case transIfaces ifaces of
                Right mDefs -> run (Spec mDefs)
                Left err -> putStrLn err
        Left err -> print err
