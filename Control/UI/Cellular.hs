
module Control.UI.Cellular(Application(), initApp, runApp, freeApp) where

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Data.List
import Control.Automaton

foreign import ccall "init_application" c_initApplication :: IO (Ptr AppPtr)
foreign import ccall "run_application" c_runApplication :: Ptr AppPtr -> CInt -> Ptr CString -> IO CInt
foreign import ccall "free_application" c_freeApplication :: Ptr AppPtr -> IO ()

foreign export ccall cPositionOf :: CInt -> CInt -> CInt -> CChar

data AppPtr

newtype Application = Application { unApplication :: Ptr AppPtr }

initApp :: IO Application
initApp = Application <$> c_initApplication

runApp :: Application -> [String] -> IO Int
runApp (Application app) args = do
  args' <- mapM newCString args
  args'' <- newArray args'
  r <- c_runApplication app (genericLength args') args''
  free args''
  mapM_ free args'
  return $ fromIntegral r

freeApp :: Application -> IO ()
freeApp = c_freeApplication . unApplication

cPositionOf :: CInt -> CInt -> CInt -> CChar
cPositionOf rule row col =
    let rule' = fromIntegral rule
        row'  = fromIntegral row
        col'  = fromIntegral col
    in if positionOf (Rule rule') row' col' then
           CChar 1
       else
           CChar 0
