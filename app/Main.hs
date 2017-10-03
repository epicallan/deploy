{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.IORef       (IORef, newIORef)
import           Data.Text        ()
import           Web.Spock        (SpockM, get, root, runSpock, spock, text)
import           Web.Spock.Config (PoolOrConn (..), defaultSpockCfg)


data MySession = EmptySession
newtype MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = get root $ text "Hello World!"
