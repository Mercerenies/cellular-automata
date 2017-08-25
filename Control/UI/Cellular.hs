
module Control.UI.Cellular(CallbackFunc, Application(), initApp, runApp, freeApp,
                           cPositionOf, cPositionOfMemo, wrapCallback) where

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Data.List
import Control.Automaton
import Control.Memo
import Control.Monad.ST

type CallbackFunc = CInt -> CInt -> CInt -> IO CChar

foreign import ccall "init_application" c_initApplication :: FunPtr CallbackFunc -> IO (Ptr AppPtr)
foreign import ccall "run_application" c_runApplication :: Ptr AppPtr -> CInt -> Ptr CString -> IO CInt
foreign import ccall "free_application" c_freeApplication :: Ptr AppPtr -> IO ()

foreign import ccall "wrapper" c_wrapCallback :: CallbackFunc -> IO (FunPtr CallbackFunc)

foreign export ccall cPositionOf :: CInt -> CInt -> CInt -> CChar

data AppPtr

newtype Application = Application (Ptr AppPtr)

initApp :: FunPtr CallbackFunc -> IO Application
initApp c = Application <$> c_initApplication c

runApp :: Application -> [String] -> IO Int
runApp (Application app) args = do
  args' <- mapM newCString args
  args'' <- newArray args'
  r <- c_runApplication app (genericLength args') args''
  free args''
  mapM_ free args'
  return $ fromIntegral r

freeApp :: Application -> FunPtr CallbackFunc -> IO ()
freeApp (Application app) ptr = freeHaskellFunPtr ptr >> c_freeApplication app

cPositionOf :: CInt -> CInt -> CInt -> CChar
cPositionOf rule row col =
    let rule' = fromIntegral rule
        row'  = fromIntegral row
        col'  = fromIntegral col
    in if positionOf (Rule rule') row' col' then
           CChar 1
       else
           CChar 0

cPositionOfMemo :: MemoTable RealWorld -> CallbackFunc
cPositionOfMemo memo rule row col =
    let rule' = fromIntegral rule
        row'  = fromIntegral row
        col'  = fromIntegral col
    in do
      result <- stToIO $ positionOfMemo memo rule' row' col'
      pure $ if result then CChar 1 else CChar 0

wrapCallback :: CallbackFunc -> IO (FunPtr CallbackFunc)
wrapCallback = c_wrapCallback
