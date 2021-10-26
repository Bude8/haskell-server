module Server where

import qualified Data.ByteString.Char8 as B
import           Network.Simple.TCP    (HostPreference (Host), send, serve)

run :: Int -> IO ()
run port = do putStrLn $ "TCP server listening on port " ++ show port ++ "..."
              serve (Host "127.0.0.1") (show port) $ \(connectionSocket, remoteAddr) -> do
                putStrLn $ "TCP connection established from " ++ show remoteAddr
                send connectionSocket $ B.pack "Hello!"
