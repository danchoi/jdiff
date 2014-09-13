{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Text.Show.Pretty as P
import System.Environment
import Data.Maybe
import qualified Data.Set as S
import Control.Monad


fileToValue :: FilePath -> IO Value
fileToValue fp = do 
  s <-  B.readFile $ fp 
  case decode s of 
    Just v -> return v
    Nothing -> error $ "Can't parse " ++ fp


diffKeys x y = do
    let x' = S.fromList (M.keys x)
        y' = S.fromList (M.keys y)
    putStrLn "Missing from right"
    print $ S.difference x' y'
    putStrLn "Missing from left"
    print $ S.difference y' x'
    let c = S.intersection x' y'
    putStrLn "Checking common values for equality..."
    forM_ (S.toList c) 
          (\a -> do
            let eq = M.lookup a x == M.lookup a y
            putStrLn $ show a ++ " => " ++ show eq
            when (not eq) $ do
              forM_ [("left", M.lookup a x), ("right", M.lookup a y)] 
                    (\(label, o) -> do
                      putStrLn label
                      B.putStrLn . encode $ o
                    )
          )


diff :: Value -> Value -> IO ()
diff (Object x) (Object y) = do
    diffKeys x y
diff x y = do
    putStrLn $ (show x) ++ " == " ++ (show y) ++ " => " ++ (show $ x == y)

main = do
    [file1, file2] <- getArgs
    o1 <- fileToValue file1
    o2 <- fileToValue file2
    putStrLn $ show (o1 == o2)
    diff o1 o2
    return ()

  
