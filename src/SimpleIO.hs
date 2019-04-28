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
                                                , Text
                                                , Eq
                                                , Show
                                                , Read
                                                , Int
                                                , (/=)
                                                , (==)
                                                )
import qualified Data.Text                     as T
import           Data.Char                      ( isAlpha )


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

data Event = Event Text Int Method Text deriving (Eq, Show, Read)

parseEvent :: Text -> Maybe Event
parseEvent ""  = Nothing
parseEvent str = Just $ Event uname time method path
 where
  getUnameStr = T.filter (/= ' ') . T.takeWhile (/= '[')
  getMethodStr =
    T.takeWhile (isAlpha) . T.dropWhile (/= ' ') . T.dropWhile (/= ']')
  getTimeStr  = T.takeWhile (/= ']') . T.drop 1 . T.drop (T.length uname)
  getPath = T.dropWhile (/= '/')
  uname       = getUnameStr str
  time        = fromMaybe 0 $ readMay . getTimeStr $ str
  method  = fromMaybe POST $ readMay . getMethodStr $ str
  path    = getPath str

