{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleIO where

import           Prelude                        ( head )
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
  path <- listToMaybe $ args


