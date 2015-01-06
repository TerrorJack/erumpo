module Main where

import qualified Data.Map as Map
import           REPL

main :: IO ()
main = do putStrLn "Welcome to Erumpo REPL!"
          repl Map.empty
