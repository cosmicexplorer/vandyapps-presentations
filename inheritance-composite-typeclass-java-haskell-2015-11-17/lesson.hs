-- -*- compile-command: "ghc *.hs -o lesson" -*-
-- run with:
-- ./lesson tcp
-- ./lesson udp

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
  -- interface functions can have default implementations!
  -- better to think of as possibly-abstract base classes
  -- if default implementation doesn't exist, all instances MUST implement
  ping _ = do return "lol"

data TCPPinger = TCPPinger {tcphost, tcpport, tcpmsg :: String} deriving Show

-- note that implementation of a typeclass can happen after a type is defined!
instance Pinger TCPPinger where
  ping pinger = TCP.connect (tcphost pinger) (tcpport pinger) $ \(sk, ad) -> do
    TCP.send sk $ pack $ tcpmsg pinger
    received <- TCP.recv sk sockByteLimit
    return $ unpackRequest received

-- named accessors in haskell create new functions, which is why we can't just
-- call these "host," "port," and "msg"
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

-- pattern matching
-- this must be exhaustive or everything dies!
displayResult :: PingResult -> String
displayResult (SuccessResult s) = "SUCCESS: " ++ s
displayResult (FailureResult s) = "FAILURE: " ++ s
displayResult (InfoResult s) = "INFO: " ++ s

-- boring implementation details
msgMatcher = mkRegex "^(Success|Failure):\\s*"
replaceResult s = subRegex msgMatcher s ""
makeResult :: String -> PingResult
makeResult s
  | startswith "Success" s = SuccessResult nonBegin
  | startswith "Failure" s = FailureResult nonBegin
  | otherwise = InfoResult s
  where nonBegin = replaceResult s

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
