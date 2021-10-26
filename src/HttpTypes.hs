module HttpTypes where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.Tuple            (swap)

data HttpStatusCode
    = HttpStatusOK
    | HttpStatusNotFound
    | HttpStatusUnknownError
    deriving (Eq, Show)

instance Enum HttpStatusCode where
    fromEnum = fromJust . flip lookup codes
    toEnum = fromJust . flip lookup (map swap codes)
codes = [(HttpStatusOK, 200), (HttpStatusNotFound, 404), (HttpStatusUnknownError, 500)]

data HttpHeader
    = HttpHeader String String
    deriving Show

data HttpResponse = HttpResponse
    { httpResponseCode    :: HttpStatusCode
    , httpResponseHeaders :: [HttpHeader]
    , httpResponseBody    :: String
    , httpVersion         :: String
    } deriving (Show)

httpStatusCodeReason :: HttpStatusCode -> String
httpStatusCodeReason HttpStatusOK       = "OK"
httpStatusCodeReason HttpStatusNotFound = "NotFound"
httpStatusCodeReason _                  = "UnknownError"

httpResponseToByteString :: HttpResponse -> BS.ByteString
httpResponseToByteString hr =
  let code = httpResponseCode hr
      headers = httpResponseHeaders hr
      body = httpResponseBody hr
      vers = httpVersion hr
  in
    B.pack (vers ++ " " ++ show (fromEnum code) ++ " " ++ httpStatusCodeReason code ++ "\n" ++
            "Content-Length: " ++ show (length body) ++ "\n" ++ getHeaders headers ++ "\n"
            ++ body)

getHeaders :: [HttpHeader] -> String
getHeaders []                      = "\n"
getHeaders [(HttpHeader k v)]      = k ++ ": " ++ v ++ "\n"
getHeaders ((HttpHeader k v) : xs) = k ++ ": " ++ v ++ "\n" ++ getHeaders xs
