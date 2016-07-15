import Session
import Spec

import qualified Data.Map as M
import JS.Type

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

main = run spec
