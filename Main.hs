
module Main where

import System.Environment
import Control.UI.Cellular
import Control.Applicative
import Control.Memo
import Control.Monad.ST
import Control.Automaton

main :: IO ()
main = do
  table <- stToIO $ memoTable (Rule 0)
  func <- wrapCallback $ cPositionOfMemo table
  app <- initApp func
  args <- liftA2 (:) getProgName getArgs
  r <- runApp app args
  freeApp app func
  putStrLn $ "Exited with " ++ show r
