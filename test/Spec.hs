
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

import           ClassyPrelude
import           SimpleIO                       ( parseEvent
                                                , Event(..)
                                                , Method(..)
                                                )
import           Data.String                    ( fromString )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Test.Hspec.Runner              ( configFastFail
                                                , defaultConfig
                                                , hspecWith
                                                )


main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } specs


specs :: Spec
specs = describe "parseEvent" $ for_ cases test
  where test Case {..} = it description $ parseEvent input `shouldBe` expected


data Case  = Case { description :: String
                  , input       :: Text
                  , expected    :: Maybe Event
                  }

cases :: [Case]
cases =
  [ Case { description = "Happy Path"
         , input       = "bobby      [1100]    POST    /index.html"
         , expected    = Just $ Event "bobby" 1100 POST "/index.html"
         }
  , Case { description = "empty line", input = "", expected = Nothing }
  ]
