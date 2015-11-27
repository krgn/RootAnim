module Main where

import Graphics.Rendering.OpenGL                                           
import qualified Graphics.UI.GLWindow as Window                            
                                                                           
myLoop = do clear [ColorBuffer]                                            
            t <- Window.time
            clearColor $= Color4 (sin (realToFrac t) * 0.5 + 0.5)          
                                 (sin (realToFrac (t+1)) * 0.5 + 0.5)      
                                 (sin (realToFrac (t+2)) * 0.5 +0.5)       
                                 0
                                                                           
main = do Window.init 3 2 -- initializes a OpenGL 3.2 context              
          Window.loop myLoop -- stops when the ESC key is pressed          
          Window.kill -- removes the window when the loop stops
