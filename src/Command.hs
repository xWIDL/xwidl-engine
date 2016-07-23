{-# LANGUAGE DeriveFunctor #-}

module Command where

import JS.Type
import JS.Platform
import Control.Monad.Free

-- Free-Monad Style DSL
data CmdF next = Eval JsExpr (JsVal -> next)
               | Invoke LVar Name [JsExpr] next
               | Assert JsExpr next
               | End
               deriving Functor

type CmdM = Free CmdF

type Commands = CmdM ()

-- Sugars

eval :: JsExpr -> CmdM JsVal
eval e = liftF (Eval e id)

new :: Name -> [JsExpr] -> CmdM JsVal
new x args = eval (JNew x args)

invoke :: LVar -> Name -> [JsExpr] -> CmdM ()
invoke l f args = liftF (Invoke l f args ())

assert :: JsExpr -> CmdM ()
assert e = liftF (Assert e ())

end :: Commands
end = liftF End
