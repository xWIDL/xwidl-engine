module Main where

import Spec
import Command
import Model
import Drive
import Dafny

import qualified Data.Map as M

{-
var foo = new Foo;
var bar = foo.spawn_bar();
// assert bar != null;
// assert foo.spawn_bars_count == 1;
-}


cmds :: Commands
cmds = do
    foo <- new (Name "Foo") []
    bar <- eval (JCall (LVal foo) (Name "spawn_bar") [])
    assert (JRel NotEqual (JVal bar) (JVal (JPrim PNull)))
    assert (JRel Equal (JAccess (LVal foo) (Name "spawn_bars_count")) (JVal (JPrim (PInt 1))))

main = drive "examples/ex-3/out.dafny" spec cmds
