module Main where

import Spec
import Command
import Model
import Drive
import Dafny

import qualified Data.Map as M

{-
var bar = new Bar();
// assert bar != null;
var foo1 = new Foo(null);
// assert foo1 == null;
var foo2 = new Foo(bar);
// assert foo2 != null;
-}


cmds :: Commands
cmds = do
    bar <- new (Name "Bar") []
    assert (JRel NotEqual (JVal bar) (JVal (JPrim PNull)))
    foo1 <- new (Name "Foo") [JVal (JPrim PNull)]
    assert (JRel Equal (JVal foo1) (JVal (JPrim PNull)))
    foo2 <- new (Name "Foo") [JVal bar]
    assert (JRel NotEqual (JVal foo2) (JVal (JPrim PNull)))

main = drive "examples/ex-2/out.dafny" spec cmds
