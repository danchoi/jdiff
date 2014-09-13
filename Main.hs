{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import Data.HashMap.Strict
import Data.Vector
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Text.Show.Pretty as P

main = do
    raw <- B.getContents
    let v = decode raw :: Maybe Value
    putStrLn $ P.ppShow v

  
