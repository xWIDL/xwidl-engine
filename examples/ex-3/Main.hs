module Main where

import Spec
import Command
import Model
import Drive
import Dafny

import qualified Data.Map as M

{-
interface Bar {};

[
Constructor
//- ensures ret != null && this.spawn_bars_count == 0
]
interface Foo {
    //- ghost attribute int spawn_bars_count;

    Bar spawn_bar();
    //- ensures ret != null && this.spawn_bars_count == old(this.spawn_bars_count) + 1;

    // Well, this is a hack, we can write this as *effects* but this needs further work
};
-}

spec :: Spec
spec = Spec (M.fromList [(Name "Bar", ifBar), (Name "Foo", ifFoo)])
    where
        ifBar = Interface (Name "Bar") [] M.empty M.empty
        ifFoo = Interface (Name "Foo")
                          [consFoo]
                          (M.fromList [(Name "spawn_bars_count", ITyInt)])
                          (M.fromList [(Name "spawn_bar", spawn_bar)])
        consFoo = InterfaceConstructor {
            _icArgs = [],
            _icRequires = Nothing,
            _icEnsures  = Just "ret != null && this.spawn_bars_count == 0"
        }
        spawn_bar = InterfaceMethod {
            _imName = Name "spawn_bar",
            _imArgs = [],
            _imRet  = Just (ITyInterface (Name "Bar")),
            _imEnsures = Just "ret != null && this.spawn_bars_count == old(this.spawn_bars_count) + 1",
            _imRequires = Nothing
            -- _imEffects = Just "this.spawn_bars_count := this.spawn_bars_count + 1;\n"
        }

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
