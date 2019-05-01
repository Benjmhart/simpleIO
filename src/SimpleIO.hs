{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleIO where

import           ClassyPrelude                  ( putStrLn
                                                , IO
                                                , print
                                                , getArgs
                                                , ($)
                                                , (.)
                                                , listToMaybe
                                                )



simpleIOMain :: IO ()
simpleIOMain = do
  args <- getArgs
  path <- listToMaybe $ args


