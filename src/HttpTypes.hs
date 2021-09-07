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
  in
    B.pack (v ++ " " ++ show (fromEnum code) ++ " " ++ httpStatusCodeReason code ++ "\n" ++
            "Content-Length: " ++ show (length body) ++ "\n" ++
            "")


h = HttpStatusOK
headers = [HttpHeader "h1" "h2", HttpHeader "h3" "h4"]
r = HttpResponse h headers "body"
v = "HTTP/1.1"

getHeaders :: HttpHeader -> String
getHeaders (HttpHeader h1 h2) = h1 ++ ": " ++ h2
