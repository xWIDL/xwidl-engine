module Main where

import Spec
import Command
import Model
import Drive

import qualified Data.Map as M

{-
interface Bar {};

interface Foo {
    void use_bar(Bar? bar);
    ///- requires bar != null
};
-}

spec :: Spec
spec = Spec (M.fromList [(Name "Bar", ifBar), (Name "Foo", ifFoo)])
    where
        ifBar = Interface (Name "Bar") [] M.empty M.empty
        ifFoo = Interface (Name "Foo") [] M.empty (M.fromList [(Name "use_bar", mUseBar)])
        mUseBar = InterfaceMethod {
            _imName = Name "use_bar",
            _imArgs = [(Name "bar", ITyNullable (ITyInterface (Name "Bar")))],
            _imRet  = Nothing,
            _imEnsures = Nothing,
            _imRequires = Just "bar != null"
            -- _imEffects = Nothing
        }

{-
Foo.use_bar(null); // bug
-}

cmds = invoke (LInterface (Name "Foo"))
              (Name "use_bar")
              [JVal (JPrim PNull)]


main = drive "examples/ex-1/out.dafny" spec cmds
