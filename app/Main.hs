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
  print =<< authGet
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

authGet :: IO ByteString
authGet = do
  -- generated this bearer with curl /v1/token at: https://instantwebtools.net/secured-fake-rest-api
  let opts = defaults & auth ?~ oauth2Bearer "eyJraWQiOiJQZjNoZlFEYVdYM0dMekJ2ZHk3MkhzVExRWUhKbk45dldFQm1LLUItUFJvIiwiYWxnIjoiUlMyNTYifQ.eyJ2ZXIiOjEsImp0aSI6IkFULnJjbHppTGR3QUtRdUQ1X3R2Smk1ZnpKQ3BTLXV2WWFiQVE4d0pUb01YRnMub2FyY2p1czlucDJVdHE0MUY0eDYiLCJpc3MiOiJodHRwczovL2Rldi00NTc5MzEub2t0YS5jb20vb2F1dGgyL2F1c2hkNGM5NVF0RkhzZld0NHg2IiwiYXVkIjoiYXBpIiwiaWF0IjoxNjMwOTU3NzAwLCJleHAiOjE2MzA5NjEzMDAsImNpZCI6IjBvYWhkaGprdXRhR2NJSzJNNHg2IiwidWlkIjoiMDB1aGVuaDFwVkRNZzJ1ZXg0eDYiLCJzY3AiOlsib2ZmbGluZV9hY2Nlc3MiXSwic3ViIjoiYXBpLXVzZXI0QGl3dC5uZXQifQ.EXwgGLVIpHvDObOyP5Xud_ayZW5Y3UuA7evDGVGog8EEFgr0nYw51WQLsrwbWjA9SYb-94822yn9gaJt74hVksAL9XKpfzTS1wIMgIKZjQCTh3XOX5167H5BHkHpkaATNTd4G4wjpWfTjClEKwpmGEMEeIAKQ-vTZgGCU5H5ABLNNpcrwDBLvWddLNSny1zABXdXDx1N4OYj8vglv7T27LljUjPwAZNy-fFF3s24fzf0EyPK7A9WngyYS1o_Ry_1PjaFIeIG0G3LjTtgoX8Kcan_CZrIpV6K8m619Ll-Jzon03aDJj8LNm8npcRhgemNDUUAHavUdb1pAvqquaIzhQ"
  resp <- getWith opts "https://api.instantwebtools.net/v2/airlines"
  return $ resp ^. responseBody
