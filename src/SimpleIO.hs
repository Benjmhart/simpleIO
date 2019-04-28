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
        (map $ first (\_ -> "File read failed."))
          . tryIOError
          . readFileUtf8
          . unpack
  fileContents <- getContentsFromPath ==<< (path)
    -- This is a bit of a cheat  - to do this for without a custom operator,
    -- Use a Monad Transformer
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

parseEvent :: Text -> Maybe Event
parseEvent ""  = Nothing
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
  time    = readMay . getTimeStr $ str
  method  = readMay . getMethodStr $ str
  path    = getPath str

