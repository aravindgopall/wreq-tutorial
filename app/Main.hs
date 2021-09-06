{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy (ByteString(..))
import GHC.Generics
import Network.Wreq

main :: IO ()
main = do
  print $ ("lens", "tut") ^. _2
  print =<< simpleGet
  print =<< simpleGetWith
  print =<< simplePost
  print =<< simplePostWith
  putStrLn "Tutorial ended..."

simpleGet :: IO ByteString
simpleGet = do
  resp <- get "http://httpbin.org/get"
  return $ resp ^. responseBody

simpleGetWith :: IO ByteString
simpleGetWith = do
  --let opts = defaults & param "foo" .~ ["bar", "quux"]
  let opts = defaults & header "Accept" .~ ["*/*"]
  resp <- getWith opts "http://httpbin.org/get"
  return $ resp ^. responseBody

data ExRequest = ExRequest
  { num :: Int
  , str :: String
  }
  deriving (Generic, ToJSON)

simplePost :: IO ByteString
simplePost = do
  let req = ExRequest 1 "one"
  resp <- post "http://httpbin.org/post" (toJSON req)
  return $ resp ^. responseBody

simplePostWith :: IO ByteString
simplePostWith = do
  let opts = defaults & param "foo" .~ ["bar", "quux"]
  let req = ExRequest 1 "one"
  resp <- postWith opts "http://httpbin.org/post" (toJSON req)
  return $ resp ^. responseBody
