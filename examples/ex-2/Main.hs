module Main where

import Spec
import Command
import Model
import Drive
import Dafny

import qualified Data.Map as M

{-
[
Constructor
//- ensures ret != null
]
interface Bar {};

[
Constructor(Bar? bar)
//- ensures if bar != null then ret != null else ret == null
]
interface Foo {};
-}

spec :: Spec
spec = Spec (M.fromList [(Name "Bar", ifBar), (Name "Foo", ifFoo)])
    where
        ifBar = Interface (Name "Bar") [consBar] M.empty M.empty
        consBar = InterfaceConstructor {
            _icArgs = [],
            _icRequires = Nothing,
            _icEnsures = Just "ret != null"
        }
        ifFoo = Interface (Name "Foo") [consFooWithBar] M.empty M.empty
        consFooWithBar = InterfaceConstructor {
            _icArgs = [(Name "bar", ITyNullable (ITyInterface (Name "Bar")))],
            _icRequires = Nothing,
            _icEnsures  = Just "if bar != null then ret != null else ret == null"
        }

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
