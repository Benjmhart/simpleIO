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
                                                )



simpleIOMain :: IO ()
simpleIOMain = do
  args <- getArgs
  putStrLn . head $ args -- head gets the first item in the list


