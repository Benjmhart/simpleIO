{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleIO where

import           ClassyPrelude                  ( putStrLn
                                                , IO
                                                )

simpleIOMain :: IO ()
simpleIOMain = putStrLn "Welcome to the Haskell Beginners class Exercise"


