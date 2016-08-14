module Util where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.PrettyPrint.Leijen (pretty, Pretty)
import Data.Char (toLower, toUpper)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM x a b = x >>= \x -> if x then a else b

prettyShow :: Pretty a => a -> String
prettyShow = show . pretty

warning :: MonadIO m => String -> m ()
warning s = liftIO (putStrLn $ "[WARNING] " ++ s)

logging :: MonadIO m => String -> m ()
logging s = liftIO (putStrLn $ "[LOGGING] " ++ s)

toLowerFirst :: String -> String
toLowerFirst (x : xs) = toLower x : xs
toLowerFirst [] = []



toUpperFirst :: String -> String
toUpperFirst (x : xs) = toUpper x : xs
toUpperFirst [] = []


