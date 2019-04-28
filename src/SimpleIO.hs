{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleIO where

import           ClassyPrelude                  ( putStrLn
                                                , IO
                                                , print
                                                , getArgs
                                                )

simpleIOMain :: IO ()
simpleIOMain = do
  args <- getArgs  -- gives use a list of arguments as a list of Text
  print args -- calls show on the text, then outputs it and returns ()


