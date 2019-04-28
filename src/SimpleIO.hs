{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleIO where

import           ClassyPrelude                  ( putStrLn
                                                , IO
                                                , print
                                                , getArgs
                                                , ($)
                                                , (.)
                                                , FilePath
                                                , listToMaybe
                                                )



simpleIOMain :: IO ()
simpleIOMain = do
  args <- getArgs
  print . listToMaybe $ args


