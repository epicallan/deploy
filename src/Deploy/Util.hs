{-# LANGUAGE OverloadedStrings #-}

module Deploy.Util where

import           Control.Exception.Safe (IOException)



handlerIO :: IOException -> IO ()
handlerIO ex = putStrLn $ "caught exception " ++ show ex

