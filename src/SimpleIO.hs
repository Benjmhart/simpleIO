{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleIO where

import           ClassyPrelude
import qualified Data.Text                     as T
import           Data.Char                      ( isAlpha
                                                , isNumber
                                                )
import           Data.Either.Utils              ( maybeToEither )
import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( sequenceA_ )
simpleIOMain :: IO ()
simpleIOMain = do
  args <- getArgs
  let path = listToMaybe $ args
  case path of
    Nothing -> putStrLn "You must supply a file path!"
    Just x  -> do
      putStrLn $ "Log Path: " <> x -- this part up here is hard because types don't align
      fileContents <- convertError . tryIOError . readFileUtf8 . unpack $ x
      let rows    = drop 1 . T.lines <$> fileContents  -- Either Text [Text]
          events  = map rights $ (parseEvent <$$> rows)
          problem = findBefore =<< events
      either (print) (print . ("Here's the problem" <>) . show) problem
      return ()


convertError =
  (map $ first (\e -> "Error: problem opening file or invalid path"))

(<$$>) = map . map


findBefore :: [Event] -> Either Text Event
findBefore []                           = Left "Ain't no problem!"
findBefore (x : (Event _ _ ERR _) : _ ) = Right x
findBefore (x                     : xs) = findBefore xs


data Method = POST | GET | ERR deriving (Eq, Show, Read)

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

parseEvent :: Text -> Either Text Event
parseEvent ""  = Left "No Parse"
parseEvent str = Event uname <$> time <*> method <*> pure path
 where
  getUnameStr = T.filter (/= ' ') . T.takeWhile (/= '[')
  getMethodStr =
    T.takeWhile (isAlpha) . T.dropWhile (== ' ') . drop 1 . T.dropWhile (/= ']')
  getTimeStr =
    T.takeWhile isNumber . T.dropWhile (not . isNumber) . T.dropWhile (/= ' ')
  getPath = T.dropWhile (/= '/')
  uname   = getUnameStr str
  time    = maybeToEither "No Parse" . readMay . getTimeStr $ str
  method  = maybeToEither "No Parse" . readMay . getMethodStr $ str
  path    = getPath str

