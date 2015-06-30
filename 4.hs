{-# LANGUAGE PackageImports #-}
import Prelude hiding (init)
import Foreign.Storable (sizeOf)
import Control.Concurrent (threadDelay)
import Control.Applicative
import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import TGA
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import System.FilePath ((</>))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Graphics.UI.GLFW
import Foreign.Ptr
import Data.Array.Storable

data Shaders = Shaders { getProgram        :: Program
                       , positionA         :: AttribLocation
                       , colorA            :: AttribLocation}

data Resources = Resources { vertexBuffer  :: BufferObject
                           , elementBuffer :: BufferObject
                           , shaders       :: Shaders
                           }

myPoints :: [ GLfloat ]
myPoints = [  0.0, 0.5, 1.0, 0.0, 0.0,
              0.5, -0.5, 0.0, 1.0, 0,
             -0.5, -0.5, 0.0, 0.0, 1.0 ]

offset = plusPtr nullPtr

initShaders = do
  vs <- loadShader VertexShader $ "4.vert"
  fs <- loadShader FragmentShader $ "4.frag"
  p <- linkShaderProgram [vs, fs]
  Shaders p
          <$> get (attribLocation p "position")
          <*> get (attribLocation p "colour")

makeResources :: IO Resources
makeResources = Resources
                <$> makeBuffer ArrayBuffer myPoints
                <*> makeBuffer ElementArrayBuffer [0..3::GLuint]
                <*> initShaders

setupGeometry :: Resources -> IO ()
setupGeometry r = let posn = positionA (shaders r)
                      colr = colorA (shaders r)
                      stride = fromIntegral $ sizeOf (undefined :: GLfloat) * 5
                      vad = VertexArrayDescriptor 2 Float stride offset0
                      col = VertexArrayDescriptor 3 Float stride $ offset (sizeOf (undefined :: GLfloat) * 2)
                  in do bindBuffer ArrayBuffer $= Just (vertexBuffer r)
                        vertexAttribPointer posn $= (ToFloat, vad)
                        vertexAttribArray posn $= Enabled
                        vertexAttribPointer colr $= (ToFloat, col)
                        vertexAttribArray colr $= Enabled
                        bindBuffer ElementArrayBuffer $= Just (elementBuffer r)

drawInit :: Resources -> IO ()
drawInit r = do clearColor $= Color4 1 1 1 1
                clear [ ColorBuffer ]
                currentProgram $= Just (getProgram (shaders r))
                setupGeometry r

draw :: Resources -> IO ()
draw r = do
  clear [ ColorBuffer ]
  drawElements Triangles 3 UnsignedInt offset0

animate :: Resources -> IO Resources
animate r = do
  return r

main :: IO ()
main = do
  ok <- init
  when (not ok) (error "Error Initializing GLFW!")
  
  mapM_ windowHint
        [ WindowHint'Samples 4 ]


  m@(~(Just w)) <- createWindow 500 500 "Hello World" Nothing Nothing
  when (isNothing m) (error "Couldn't create window!")
  makeContextCurrent m
  setWindowTitle w "Hello World"
  r0 <- makeResources
  drawInit r0
  let keyIsPressed k = (== KeyState'Pressed) <$> getKey w k
      go r = do draw r
                swapBuffers w
                pollEvents
                closeWindow <- windowShouldClose w
                keyIsPressed Key'Escape >>= flip unless (animate r >>= go)
  go r0
