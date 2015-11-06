import qualified Data.ByteString.Char8 as C
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord

data Status  = On {availableStatus :: Int} | Off deriving (Show)

data Server = Server { key    :: Int
                     , ratio  :: Int
                     , status :: Status
                     , total  :: Int
                     } deriving (Show)

data Request = Switch {switch :: Int} | Request {request :: Int} deriving (Show)

--------

isNotRequest (Switch _) = True
isNotRequest _ = False

fromRequest (Switch x)  = x
fromRequest (Request x) = x

--------

isStatusOn (Server _ _ (On _) _) = True
isStatusOn _ = False

isNotStatusOn = not . isStatusOn

fromStatus server = availableStatus $ status server

switchStatus (Server k r (On _) t) = (Server k r Off t)
switchStatus server = server {status = On (ratio server)}

-- Switching status from off to on refreshes the server's available ratio
reloadServerRatio server
    | isStatusOn server = switchStatus $ switchStatus server
    | otherwise         = switchStatus server

receiveServerRequest server req =
    let newAvailableStatus = (availableStatus $ status server) - req
        newTotal           = total server + req
    in server {status = On newAvailableStatus, total = newTotal}

receiveAndReload s req = reloadServerRatio $ receiveServerRequest s req

--------

-- Switch a specific server
switchServerByKey (server:ss) serverKey
    | key server == serverKey = switchStatus server : ss
    | otherwise               = server : switchServerByKey ss serverKey

-- Rotatively distribute a request by server
distributeRequest servers (Request 0) = servers
distributeRequest servers@(s:ss) request
    | isNotRequest request  = switchServerByKey servers (switch request)
    | isNotStatusOn s       = distributeRequest (ss ++ [s]) request
    | availableRatio >= req = receiveServerRequest s req : ss
    | otherwise             = distributeRequest (ss ++ [receiveAndReload s availableRatio]) reqNew
    where availableRatio = fromStatus s
          req = fromRequest request
          reqNew = Request (req - availableRatio)

-- Distribute each call through the servers
distributeCalls servers [] = servers
distributeCalls servers calls@(c : cs) = distributeCalls serversNew cs
    where serversNew = distributeRequest servers c

--------

createServers [] _ = []
createServers ratios@(r:rs) k = (Server k sr (On sr) 0) : createServers rs (k + 1)
    where sr = fst . fromJust . C.readInt $ r

createCalls [] = []
createCalls (c:cs)
    | length c == 1 = (Request request) : createCalls cs
    | otherwise     = (Switch serverKey) : createCalls cs
    where request   = fromByteString $ head c
          serverKey = fromByteString $ c!!1

--------

fromByteString = fst . fromJust . C.readInt

parseForOutput = map total . sortBy (comparing key)

--------

main = do
  t <- C.getLine
  serversRatios <- C.words <$> C.getLine
  input <- fromByteString <$> C.getLine
  callsByte  <- replicateM input $ do
     C.words <$> C.getLine
  let servers = createServers serversRatios 1
  let calls = createCalls callsByte
  let results = distributeCalls servers calls
  putStrLn . unlines . map show $ parseForOutput results
