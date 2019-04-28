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
                                                , Maybe(..)
                                                , Either(..)
                                                , (<>)
                                                , tshow
                                                , readFileUtf8
                                                , return
                                                , unpack
                                                , tryIOError
                                                , listToMaybe
                                                , String
                                                , Eq
                                                , Show
                                                , Read
                                                , Int
                                                )
import qualified Data.Text                     as T
  -- you need to add Text to package.yaml


simpleIOMain :: IO ()
simpleIOMain = do
  args <- getArgs
  let path = listToMaybe $ args
  case path of
    Nothing -> putStrLn "You must supply a file path!"
    Just x  -> do
      putStrLn $ "Log Path: " <> x
      fileContents <- tryIOError . readFileUtf8 . unpack $ x
      case fileContents of
        Left  e -> print $ "Error: " <> tshow e
        Right c -> do
          let rows = T.lines c
          return ()


data Method = Post | Err deriving (Eq, Show, Read)

data Event = Event String Int Method String
