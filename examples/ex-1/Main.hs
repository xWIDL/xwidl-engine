module Main where

import Spec
import Command
import Model
import Drive

import qualified Data.Map as M

{-
Foo.use_bar(null); // bug
-}

cmds = invoke (LInterface (Name "Foo"))
              (Name "use_bar")
              [JVal (JPrim PNull)]


main = drive "examples/ex-1/out.dafny" spec cmds
