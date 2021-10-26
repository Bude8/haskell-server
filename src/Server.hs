module Server where

import qualified Data.ByteString.Char8 as B
import           Network.Simple.TCP    (HostPreference (Host), send, serve, recv)
import HttpTypes
import Data.Maybe (fromJust)

run :: Int -> IO ()
run port = do putStrLn $ "TCP server listening on port " ++ show port ++ "..."
              let msg = HttpResponse HttpStatusOK [HttpHeader "Content-Type" "text/html"] "<h1>this is my body *pulls out bread*, this is my blood *pulls out wine*, this is my mayonnaise</h1>" "HTTP/1.1"         
              serve (Host "127.0.0.1") (show port) $ \(connectionSocket, remoteAddr) -> do
                putStrLn $ "TCP connection established from " ++ show remoteAddr
                send connectionSocket $ httpResponseToByteString msg
                x <- recv connectionSocket 4096
                print (fromJust x)