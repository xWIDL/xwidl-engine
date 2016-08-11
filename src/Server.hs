module Control.JS.Utils.Server where

import Network.Simple.TCP
import Network.Socket (socketToHandle)

startServe f = serve (Host "localhost") "8888" $ \(sock, addr) -> do
    putStrLn $ "Listening TCP connections from " ++ show addr
    handler <- socketToHandle sock ReadWriteMode
    f handler
