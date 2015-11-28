{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foreign.C.Types
import Graphics.Rendering.OpenGL                                           
import qualified Graphics.UI.GLWindow as Window                            

thing :: forall t a b b1 b2 b3.
        (Fractional a, Fractional b, Fractional b1, Fractional b2,
         Fractional b3, Real a) =>
        (b -> CFloat)
        -> (b1 -> CFloat)
        -> (b2 -> CFloat)
        -> (b3 -> CFloat)
        -> (t -> t -> CFloat)
        -> (t -> t -> CFloat)
        -> (t -> t -> CFloat)
        -> (t -> t -> CFloat)
        -> a
        -> t
        -> IO ()
thing pr1 pr2 pr3 pr4 op1 op2 op3 op4 t fac = do
  (x :: Int) <- Window.pointer_x
  (y :: Int) <- Window.pointer_y

  let ti = t * 0.1
  let t1 = t + fromIntegral x * 0.001
  let t2 = t + fromIntegral y * 0.001
            
  clearColor $= Color4 (0.0 :: CFloat) 0.0 0.0 0.1
  _ <- color (Color4 (pr1 (realToFrac ti) :: CFloat) (pr2 (realToFrac ti)) (pr3 (realToFrac ti)) (pr4 (realToFrac ti) * 0.01))

  let x1 = (pr1 (realToFrac ti)  * op1 fac fac)  :: CFloat
  let y1 = (pr2 (realToFrac t1) / op2 fac fac)  :: CFloat
  let x2 = (pr3 (realToFrac t2) * op3 fac fac) :: CFloat
  let y2 = (pr4 (realToFrac ti)  / op4 fac fac) :: CFloat
           
  renderPrimitive Polygon $ do
      vertex (Vertex2 x1 y1)
      vertex (Vertex2 x2 y1)
      vertex (Vertex2 x2 y2)
      vertex (Vertex2 x1 y2)

                                                                           
myLoop :: IO ()
myLoop = do
  clear [ColorBuffer]                                            
  t <- Window.time
  mapM_ (thing sin cos tan atan  (+) (+) (+) (+) t) [-0.9, -0.8, -0.7, -0.6, -0.5 -0.4, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
  mapM_ (thing cos sin atan tan  (+) (+) (+) (+) t) [-1.9, -0.8, -0.7, -0.6, -0.5 -0.4, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.9]
  mapM_ (thing tan sin cos atan  (-) (-) (-) (-) t) [-1.9, -0.8, -0.7, -0.6, -0.5 -0.4, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.9]
  mapM_ (thing cos sin atan tan  (-) (-) (-) (-) t) [-0.9, -0.8, -0.7, -0.6, -0.5 -0.4, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
  mapM_ (thing sin cos  tan atan (*) (*) (*) (*) t) [-1.9, -0.8, -0.7, -0.6, -0.5 -0.4, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.9]
  mapM_ (thing sin tan atan cos  (*) (*) (*) (*) t) [-1.9, -0.8, -0.7, -0.6, -0.5 -0.4, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.9]

main :: IO ()
main = do
  _ <- Window.init 3 2 -- initializes a OpenGL 3.2 context              
  Window.loop myLoop -- stops when the ESC key is pressed          
  Window.kill -- removes the window when the loop stops
