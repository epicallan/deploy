{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Http
import           Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"
