module Util (logWarn) where
import           System.IO (hPutStrLn, stderr)

logWarn :: String -> IO ()
logWarn = hPutStrLn stderr . ("[WARN] " ++)
