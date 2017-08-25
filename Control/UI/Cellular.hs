
module Control.UI.Cellular(CallbackFunc, Application(), initApp, runApp, freeApp,
                           cPositionOf, cPositionOfMemo, wrapCallback) where

-- This module interfaces with C and manages the UI.

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Data.List
import Control.Automaton
import Control.Memo
import Control.Monad.ST

-- To facilitate memoization, we have a callback function to which C
-- stores a pointer. This shall be its type.
type CallbackFunc = CInt -> CInt -> CInt -> IO CChar

-- Functions defined in C which manage the GUI.
foreign import ccall "init_application" c_initApplication :: FunPtr CallbackFunc -> IO (Ptr AppPtr)
foreign import ccall "run_application" c_runApplication :: Ptr AppPtr -> CInt -> Ptr CString -> IO CInt
foreign import ccall "free_application" c_freeApplication :: Ptr AppPtr -> IO ()

-- Haskell will generate a function called `c_wrapCallback` for us, of
-- the type shown here. When called, it will construct a function
-- pointer from an ordinary Haskell function.
foreign import ccall "wrapper" c_wrapCallback :: CallbackFunc -> IO (FunPtr CallbackFunc)

-- We export the non-memoized version of the automaton function, just
-- in case we need it.
foreign export ccall cPositionOf :: CInt -> CInt -> CInt -> CChar

-- Phantom type for pointers to the GTK application.
data AppPtr

-- The application itself is a pointer to a GTK type.
newtype Application = Application (Ptr AppPtr)

-- Initializes the application and sets the memoization callback routine.
initApp :: FunPtr CallbackFunc -> IO Application
initApp c = Application <$> c_initApplication c

-- Starts the GUI and runs the program, given the command line arguments.
runApp :: Application -> [String] -> IO Int
runApp (Application app) args = do
  args' <- mapM newCString args
  args'' <- newArray args'
  r <- c_runApplication app (genericLength args') args''
  free args''
  mapM_ free args'
  return $ fromIntegral r

-- Frees the application pointer and the callback routine.
freeApp :: Application -> FunPtr CallbackFunc -> IO ()
freeApp (Application app) ptr = freeHaskellFunPtr ptr >> c_freeApplication app

-- This behaves identically to `positionOf` except that it uses C data
-- types.
cPositionOf :: CInt -> CInt -> CInt -> CChar
cPositionOf rule row col =
    let rule' = fromIntegral rule
        row'  = fromIntegral row
        col'  = fromIntegral col
    in if positionOf (Rule rule') row' col' then
           CChar 1
       else
           CChar 0

-- This behaves identically to `positionOfMemo` except that it uses C
-- data types.
cPositionOfMemo :: MemoTable RealWorld -> CallbackFunc
cPositionOfMemo memo rule row col =
    let rule' = fromIntegral rule
        row'  = fromIntegral row
        col'  = fromIntegral col
    in do
      result <- stToIO $ positionOfMemo memo rule' row' col'
      pure $ if result then CChar 1 else CChar 0

-- Wraps the callback routine in a function pointer.
wrapCallback :: CallbackFunc -> IO (FunPtr CallbackFunc)
wrapCallback = c_wrapCallback
