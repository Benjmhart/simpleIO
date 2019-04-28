{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleIO where

import           ClassyPrelude
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
          let rows   = drop 1 $ T.lines c
              events = catMaybes $ parseEvent <$> rows
          case events of
            [] -> print "No Events in log!"
            xs -> do
              let problem = findBefore xs
              case problem of
                Nothing -> putStrLn "Ain't no problem!"
                Just p  -> do
                  print $ "the problem is: " <> tshow p
                  return ()

findBefore :: [Event] -> Maybe Event
findBefore []                           = Nothing
findBefore (x : (Event _ _ ERR _) : _ ) = Just x
findBefore (x                     : xs) = findBefore xs


data Method = POST | GET | ERR deriving (Eq, Show, Read)

data Event = Event Text Int Method Text deriving (Eq)

instance Show Event where 
  show (Event u t m p) = "Event " <> T.unpack u <> " " <> show t <> " " <> show m <> " " <> T.unpack p   

parseEvent :: Text -> Maybe Event
parseEvent ""  = Nothing
parseEvent str = Just $ Event uname time method path
 where
  getUnameStr = T.filter (/= ' ') . T.takeWhile (/= '[')
  getMethodStr =
    T.takeWhile (isAlpha) . T.dropWhile (== ' ') . drop 1 . T.dropWhile (/= ']')
  getTimeStr = T.takeWhile (/= ']') . T.drop 1 . T.drop (T.length uname)
  getPath    = T.dropWhile (/= '/')
  uname      = getUnameStr str
  time       = fromMaybe 0 $ readMay . getTimeStr $ str
  method     = fromMaybe POST $ readMay . getMethodStr $ str
  path       = getPath str

