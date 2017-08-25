
module Main where

import System.Environment
import Control.UI.Cellular
import Control.Applicative
import Control.Memo
import Control.Monad.ST
import Control.Automaton

main :: IO ()
main = do
  -- Construct a memoization table.
  table <- stToIO $ memoTable (Rule 0)
  -- We need C to call our memoized function with an STRef that we
  -- construct in Haskell. We can't pass STRef values to C in any
  -- trivial way, so we partially apply the memoized function and pass
  -- the closure to C as a callback routine.
  func <- wrapCallback $ cPositionOfMemo table
  -- Initialize the application.
  app <- initApp func
  -- Run the application with the GUI.
  args <- liftA2 (:) getProgName getArgs
  r <- runApp app args
  -- Free and exit.
  freeApp app func
  putStrLn $ "Exited with " ++ show r
