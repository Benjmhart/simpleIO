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
                                                , drop
                                                , show
                                                , readMay
                                                , fromMaybe
                                                , not
                                                , Text
                                                , Eq
                                                , Show
                                                , Read
                                                , Int
                                                , (/=)
                                                , (==)
                                                )
import qualified Data.Text                     as T
import           Data.Char                      ( isAlpha
                                                , isNumber
                                                )


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
             -- split file into lines and get rid of the header row
          let rows = drop 1 $ T.lines c -- drop is safe, tail is not
          -- we need to map a function over each row to parse it into our types
          return ()

 -- test with ghci


data Method = POST | Err deriving (Eq, Show, Read)

data Event = Event Text Int Method Text deriving (Eq)

instance Show Event where
  show (Event u t m p) =
    "Event "
      <> T.unpack u
      <> " "
      <> show t
      <> " "
      <> show m
      <> " "
      <> T.unpack p

parseEvent :: Text -> Maybe Event
parseEvent ""  = Nothing
parseEvent str = Just $ Event uname time method path
 where
  getUnameStr = T.filter (/= ' ') . T.takeWhile (/= '[')
  uname       = getUnameStr str
  getTimeStr =
    T.takeWhile isNumber . T.dropWhile (not . isNumber) . T.dropWhile (/= ' ')
  time = fromMaybe 0 $ readMay . getTimeStr $ str
  getMethodStr =
    T.takeWhile (isAlpha) . T.dropWhile (== ' ') . drop 1 . T.dropWhile (/= ']')
  method  = fromMaybe POST $ readMay . getMethodStr $ str
  getPath = T.dropWhile (/= '/')
  path    = getPath str
