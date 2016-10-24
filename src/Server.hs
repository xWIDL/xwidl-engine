module Server where

import Language.JS.Type
import Language.JS.Platform

import Prelude hiding (div)
import Text.Read (readMaybe)
import Data.String.Utils
import Control.Monad (msum)
import Data.Either.Utils (maybeToEither)
import Control.Monad.IO.Class (liftIO)

type Hash = Int

true :: JAssert
true = JAssert (Name "x") (JEPrim (PBool True))

ifEqJust :: Eq a => a -> a -> Maybe a
ifEqJust a1 a2
  | a1 == a2 = Just a1
  | a1 /= a2 = Nothing

ifEqThen :: Eq a => a -> a -> b -> Maybe b
ifEqThen a1 a2 b
  | a1 == a2 = Just b
  | a1 /= a2 = Nothing

parseSingleVal :: String -> Either String JsVal
parseSingleVal "Bool" = Right (JVPrim PTyBool true)
parseSingleVal "Num" = Right (JVPrim PTyNumber true)
parseSingleVal "Str" = Right (JVPrim PTyString true)
parseSingleVal "Null" = Right (JVPrim PTyNull true)
parseSingleVal "UInt" = Right (JVPrim PTyInt true)
parseSingleVal "NotUInt" = Right (JVPrim PTyNumber true) -- XXX: too coarse
parseSingleVal s =
    maybeToEither s $ msum [ JVConst . PString <$> readMaybe s
                           , JVConst . PBool <$> msum [ ifEqThen "true" s True
                                                      , ifEqThen "false" s False ]
                           , JVConst . PNumber <$> readMaybe s
                           , JVConst . PInt <$> ((readMaybe s :: Maybe Double) >>=
                                                 (\n -> round <$> ifEqJust (fromIntegral (round n)) n)) ]

parseVal :: String -> Either String JsUnionVal
parseVal s = let vals = split "|" s in
             JsUnionVal <$> sequence (map parseSingleVal vals)


x = JEVar  (Name "x")
-- FIXME: a good representation for numbers/integers?
domains :: Domains
domains = Domains [(PTyBool  , [ "x" .@ (x .== JEPrim (PBool True))
                               , "x" .@ (x .== JEPrim (PBool False))])]

