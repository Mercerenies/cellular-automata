
module Main where

import System.Environment
import Control.UI.Cellular
import Control.Applicative

main :: IO ()
main = do
  app <- initApp
  args <- liftA2 (:) getProgName getArgs
  r <- runApp app args
  freeApp app
  putStrLn $ "Exited with " ++ show r
