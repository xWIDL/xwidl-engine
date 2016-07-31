-- Runtime Model
module Model where

import qualified Data.Map as M
import Language.JS.Type

-- Platform objects
data JsObj = JsObj {
    _interface :: Name
}

-- Abstract heap
data Heap = Heap {
  _mapping :: M.Map Int JsObj,
  _nextRef :: Int
}

initHeap :: Heap
initHeap = Heap {
  _mapping = M.empty,
  _nextRef = 0
}

alloc :: JsObj -> Heap -> (JRef, Heap)
alloc o (Heap m r) = (JRef r, Heap {
    _mapping = M.insert r o m,
    _nextRef = r + 1
  })

deref :: JRef -> Heap -> Maybe JsObj
deref (JRef r) (Heap m _) = M.lookup r m
