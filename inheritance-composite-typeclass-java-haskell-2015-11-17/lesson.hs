module Main where
import qualified Network.Simple.TCP as TCP
import qualified Network.Socket as SK
import qualified Network.Socket.ByteString as BSK
import Data.ByteString.Char8 hiding (putStr)
import Text.Regex
import Data.List.Utils
import System.Environment
import Control.Exception

sockByteLimit = 100

unpackRequest Nothing = ""
unpackRequest (Just s) = unpack s

-- interfaces vs typeclasses
class Pinger a where
  ping :: a -> IO String

data TCPPinger = TCPPinger {tcphost, tcpport, tcpmsg :: String} deriving Show

instance Pinger TCPPinger where
  ping pinger = TCP.connect (tcphost pinger) (tcpport pinger) $ \(sk, ad) -> do
    TCP.send sk $ pack $ tcpmsg pinger
    received <- TCP.recv sk sockByteLimit
    return $ unpackRequest received

data UDPPinger = UDPPinger {udphost, udpport, udpmsg :: String} deriving Show

instance Pinger UDPPinger where
  ping pinger =
    let host = udphost pinger
        port = udpport pinger
        msg = udpmsg pinger
    in SK.withSocketsDo $ do
      (addr:_) <- SK.getAddrInfo Nothing (Just host) (Just port)
      sock <- SK.socket SK.AF_INET SK.Datagram SK.defaultProtocol
      SK.connect sock (SK.addrAddress addr)
      BSK.sendTo sock (pack msg) (SK.addrAddress addr)
      (received, _) <- BSK.recvFrom sock sockByteLimit
      return $ unpack received

-- adt (composite types) vs subclasses
data PingResult =
  SuccessResult String
  | FailureResult String
  | InfoResult String

displayResult :: PingResult -> String
displayResult (SuccessResult s) = "SUCCESS: " ++ s
displayResult (FailureResult s) = "FAILURE: " ++ s
displayResult (InfoResult s) = "INFO: " ++ s

msgMatcher = mkRegex "^(Success|Failure):\\s*"
replaceResult s = subRegex msgMatcher s ""
makeResult :: String -> PingResult
makeResult s
  | startswith "Success" s = SuccessResult nonBegin
  | startswith "Failure" s = FailureResult nonBegin
  | otherwise = InfoResult s
  where nonBegin = replaceResult s

-- boring implementation details
data ArgException = ArgException deriving (Show)
instance Exception ArgException

sendMsg = "hey\n"
outAddr = "127.0.0.1"

main = do
  args <- getArgs
  result <- case args of
    (conn:_) ->
      if conn == "tcp" then ping $ TCPPinger outAddr "8080" sendMsg
      else ping $ UDPPinger outAddr "8081" sendMsg
    _ -> throw ArgException
  putStr $ displayResult $ makeResult result
