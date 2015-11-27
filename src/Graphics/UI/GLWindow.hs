{-# LANGUAGE ForeignFunctionInterface #-}
-- |                                                                            
-- Module      : Graphics.UI.GLWindow
-- Copyright   : (c) 2011 Hugo Gomes
--                                                                              
-- License     : BSD-style                                                      
-- Maintainer  : mr.hugo.gomes@gmail.com
-- Stability   : experimental                                                   
-- Portability : GHC                                                            
--                                                                              
-- A simple OpenGL window creator that runs on X.Org with libX11.
-- This module allows the creation of different versions of OpenGL contexts
-- by initializing with the required versions.
--                                                                              
-- /Note/: The Haskell only has bindings up to OpenGL 3.2 
--    This module is intended to be imported qualified, to avoid clashes with      
--   Prelude functions, e.g.                                                      
--
--   > import qualified Graphics.UI.GLWindow as Window                            
--                                                                               
--   As an example, here is a simple module that uses some of these functions     
--   to open a OpenGL 3.2 Context:                                                
--                                                                               
--   > module Main where                                                          
--   >                                                                            
--   > import Graphics.Rendering.OpenGL                                           
--   > import qualified Graphics.UI.GLWindow as Window                            
--   >                                                                            
--   > myLoop = do clear [ColorBuffer]                                            
--   >             t <- Window.time                                               
--   >             clearColor $= Color4 (sin (realToFrac t) * 0.5 + 0.5)          
--   >                                  (sin (realToFrac (t+1)) * 0.5 + 0.5)      
--   >                                  (sin (realToFrac (t+2)) * 0.5 +0.5)       
--   >                                  0                                              
--   >                                                                            
--   > main = do Window.init 3 2 -- initializes a OpenGL 3.2 context              
--   >           Window.loop myLoop -- stops when the ESC key is pressed          
--   >           Window.kill -- removes the window when the loop stops            
--   

module Graphics.UI.GLWindow 
    (
    -- * Types
    LoopFunc
    -- * Window initialization
    , init
    -- * Window elimination
    , kill
    -- * Window state query
    , time
    , dtime
    -- * Loop
    , loop
    ) where

import Foreign.C
import Foreign.Ptr
import Control.Applicative
import Prelude hiding (init)

foreign import ccall unsafe "WindowInit" 
  c_init :: CUInt -> CUInt -> IO (CInt)
init :: Integer -> Integer -> IO (Integer)
init glMajor glMinor = fromIntegral <$> c_init (fromIntegral glMajor) (fromIntegral glMinor)
foreign import ccall unsafe "WindowKill" 
  kill :: IO ()

foreign import ccall unsafe "WindowTime" 
  c_time   :: IO (CDouble)
time :: IO (Double)
time = realToFrac <$> c_time
foreign import ccall unsafe "WindowDTime" 
  c_dtime :: IO (CDouble)
dtime :: IO (Double)
dtime = realToFrac <$> c_dtime

type LoopFunc = IO ()
foreign import ccall "WindowLoop" 
  c_loop :: FunPtr (LoopFunc) -> IO ()
loop :: LoopFunc -> IO ()
loop lf = mkLoop lf >>= c_loop 

foreign import ccall "wrapper"
  mkLoop :: LoopFunc -> IO( FunPtr (LoopFunc) )
