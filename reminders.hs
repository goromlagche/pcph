import           Control.Concurrent
import           Control.Monad
import           Data.Time
import           Text.Printf

main :: IO ()
main = loop
  where
    loop = do
      s <- getLine
      if s == "exit"
        then do
          time <- getZonedTime
          printf "[%s] see you when I see you \n" (show time)
        else do
          _ <- forkIO $ setReminder s
          loop

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  beforeTime <- getZonedTime
  printf "[%s] Ok I will remind you in [%d] seconds \n" (show beforeTime) t
  threadDelay(10^6 * t)
  afterTime <- getZonedTime
  printf "[%s] %d seconds is up! BING!\BEL\n" (show afterTime) t
