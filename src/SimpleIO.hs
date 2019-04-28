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
  -- need to install MissingH and add missingh to package.yml
simpleIOMain :: IO ()
simpleIOMain = do
  args <- getArgs
  let path = maybeToEither "You must supply a file path!" . listToMaybe $ args
  print $ "Log Path: " <> tshow (path :: Either Text Text)
  let getContentsFromPath :: Text -> IO (Either Text Text)
      getContentsFromPath =
        (map $ first (\e -> "Error: " <> tshow e))
          . tryIOError
          . readFileUtf8
          . unpack
  fileContents <- getContentsFromPath ==<< (path)
  let rows    = drop 1 . T.lines <$> fileContents
      events  = map rights $ parseEvent <$$> rows
      problem = findBefore =<< events
  print $ "Here's the problem " <> tshow problem
  return ()

findBefore :: [Event] -> Either Text Event
findBefore []                           = Left "No Error Found in Log"
findBefore (x : (Event _ _ ERR _) : _ ) = Right x
findBefore (x                     : xs) = findBefore xs

(<$$>) = map . map

(==<<) -- 'nested bind'
  :: (Text -> IO (Either Text Text))
  -> Either Text Text
  -> IO (Either Text Text)
_ ==<< (Left  t) = pure $ Left t
f ==<< (Right a) = f a


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
                -- (liftM2 Event uname) time method path


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

