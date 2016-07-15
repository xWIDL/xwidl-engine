-- Test drive
module Drive where

import Spec
import Command
import Session
import Text.PrettyPrint.Leijen
import System.IO

-- drive :: FilePath -> Spec -> Commands -> IO ()
-- drive path spec cmds = withFile path WriteMode $ \handle -> do
--     (traits, tlms) <- run spec cmds
--     hPutStrLn handle "// ===== Traits ======"
--     mapM_ (hPutStrLn handle . show . pretty) traits
--     hPutStrLn handle "// ===== TopLevelMethods ====="
--     mapM_ (hPutStrLn handle . show . pretty) tlms

