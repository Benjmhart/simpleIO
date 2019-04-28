{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleIO where

import           ClassyPrelude                  ( putStrLn
                                                , IO
                                                )



 -- () - pronounced 'Unit'
simpleIOMain :: IO ()
simpleIOMain = putStrLn "Welcome to the Haskell Beginners class Exercise"
  -- putStrLn, takes a string, outputs it to the terminal respecting newlines, and returns ()

