import qualified Data.ByteString.Char8 as C
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord

data Status  = On Int | Off deriving (Show)

data Server = Server { key    :: Int
                     , ratio  :: Int
                     , status :: Status
                     , total  :: Int
                     } deriving (Show)

data Request = Switch {switch :: Int} | Request {request :: Int} deriving (Show)

isRequest (Request _) = True
isRequest _ = False

fromRequest (Switch x) = x
fromRequest (Request x) = x

--------

isStatusOn (Server _ _ Off _) = False
isStatusOn _ = True

fromStatus (Server _ _ (On availableStatus) _) = availableStatus

switchStatus (Server k r (On _) t) = (Server k r Off t)
switchStatus (Server k ratio Off t) = (Server k ratio (On ratio) t)

-- Switching status from off to on refreshes the server's available ratio
reloadServerRatio s
    | isStatusOn s = switchStatus $ switchStatus s
    | otherwise    = switchStatus s

receiveServerRequest :: Server -> Int -> Server
receiveServerRequest (Server k r (On st) t) req = (Server k r (On (st - req)) (t + req))

receiveReload s req= reloadServerRatio $ receiveServerRequest s req

-- Rotatively distribute a request by server
distributeRequest servers@(s:ss) request
    | not (isRequest request) && fromRequest request == key s =  switchStatus s : ss
    | not (isRequest request) ||
      not (isStatusOn s) = distributeRequest (ss ++ [s]) request
    | availableRatio > req  = receiveServerRequest s req : ss
    | availableRatio == req = ss ++ [receiveReload s req]
    | otherwise             = distributeRequest (ss ++ [receiveReload s availableRatio]) reqNew
    where availableRatio = fromStatus s
          req = fromRequest request
          reqNew = Request (req - availableRatio)

-- Distribute each call through the servers
distributeCalls servers [] = servers
distributeCalls servers calls@(c : cs) = distributeCalls serversNew cs
    where serversNew = distributeRequest servers c

--

createServers [] _ = []
createServers ratios@(r:rs) k = (Server k sr (On sr) 0) : createServers rs (k + 1)
    where sr = fst . fromJust . C.readInt $ r

createCalls [] = []
createCalls ([s,serverByte]:t) = (Switch server) : createCalls t
    where server = fromByteString serverByte
createCalls ([ratioByte]:t)    = (Request ratio) : createCalls t
    where ratio = fromByteString ratioByte

--

fromByteString = fst . fromJust . C.readInt

getReceivedRequests = map total . sortBy (comparing key)
--

main = do
  t <- C.getLine
  serversRatios <- C.words <$> C.getLine
  input <- fromByteString <$> C.getLine
  callsByte  <- replicateM input $ do
     C.words <$> C.getLine
  let servers = createServers serversRatios 1
  let calls = createCalls callsByte
  let results = distributeCalls servers calls
  putStrLn . unlines . map show $ getReceivedRequests results

