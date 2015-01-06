module REPL where

import           Common
import           Control.Monad
import qualified Data.Map      as Map
import           Interpreter
import           Parser
import           System.IO

type PEnv = Map.Map Pat Exp

readprogs :: [String] -> IO PEnv
readprogs [] = return Map.empty
readprogs (p:ps) = do penv <- readprog p
                      penvs <- readprogs ps
                      return (Map.union penvs penv)

fromimp :: Dec -> String
fromimp (ImportDec path) = path
fromimp _ = error "type error"

readprog :: String -> IO PEnv
readprog path = do handle <- openFile path ReadMode
                   contents <- hGetContents handle
                   case parse (star (do whitespace
                                        imp <- importdec
                                        return imp)) contents of
                        Just (imps,remain) -> do penv <- readprogs (map fromimp imps)
                                                 case parse (do decs <- star (do whitespace
                                                                                 dec <- definedec
                                                                                 return dec)
                                                                whitespace
                                                                return decs) remain of
                                                     Just (decs,[]) -> return (foldl (\penv (DefineDec p e)->Map.insert p e penv) penv decs)
                                                     Just (_,_) -> error "syntax error"

repl :: PEnv -> IO ()
repl env = do putStr "λ >>> "
              s <- getLine
              case parse (do whitespace
                             imp <- importdec
                             whitespace
                             return imp) s of
                   Just (ImportDec path,[]) -> do nenv <- readprog path
                                                  putStrLn ("Successfully imported "++(show (Map.size nenv))++" entries from "++path++".")
                                                  repl (Map.union nenv env)
                   Just (ImportDec _,_) -> do putStrLn "Invalid import declaration."
                                              repl env
                   Nothing -> case parse (do whitespace
                                             dec <- definedec
                                             whitespace
                                             return dec) s of
                                   Just (DefineDec p e,[]) -> do putStrLn "Successfully defined 1 entry."
                                                                 repl (Map.insert p e env)
                                   Just (DefineDec _ _,_) -> do putStrLn "Invalid define declaration."
                                                                repl env
                                   Nothing -> case parse (do whitespace
                                                             e <- expr
                                                             whitespace
                                                             return e) s of
                                                   Just (e,[]) -> case eval (LetrecExp (Map.toList env) e) Map.empty of
                                                                       Just v -> do print v
                                                                                    repl env
                                                                       Nothing -> do putStrLn "Error while evaluation."
                                                                                     repl env
                                                   _ -> do putStrLn "Invalid expression."
                                                           repl env
