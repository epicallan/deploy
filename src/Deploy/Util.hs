module Deploy.Util where

import           Control.Exception.Safe (IOException)
import           Protolude



handlerIO :: IOException -> IO ()
handlerIO ex = putStrLn $ "caught exception " ++ show ex

