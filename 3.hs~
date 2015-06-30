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

data Shaders = Shaders { getProgram        :: Program
                       , fadeFactorU    :: UniformLocation
                       , texturesU      :: [ UniformLocation ]
                       , positionA      :: AttribLocation }

data Resources = Resources { vertexBuffer  :: BufferObject
                           , elementBuffer :: BufferObject
                           , textures      :: [ TextureObject ] 
                           , shaders       :: Shaders
                           , fadeFactor    :: GLfloat }

vertexBufferData :: [ GLfloat ]
vertexBufferData = [-1, -1, 1, -1, -1, 1, 1, 1]

elementBufferData :: [ GLuint ]
elementBufferData = [0..3]

myPoints = do
   currentColor $= Color4 1 0 0 1
   vertex$Vertex3 (-0.5) (-0.5) (0.0 :: GLfloat)
   currentColor $= Color4 0 1 0 1
   vertex$Vertex3 (0.5) (-0.5) (0.0 :: GLfloat)
   currentColor $= Color4 0 0 1 1
   vertex$Vertex3 (-0.5) (0.5) (0.0 :: GLfloat)

makeTexture :: FilePath -> IO TextureObject
makeTexture filename =
    do (width, height, pixels) <- readTGA filename
       texture <- loadTexture $ texInfo width height TexBGR pixels
       textureFilter Texture2D $= ((Linear', Nothing), Linear')
       textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
       textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
       return texture

initShaders = do
  vs <- loadShader VertexShader $ "hello-gl.vert"
  fs <- loadShader FragmentShader $ "hello-gl.frag"
  p <- linkShaderProgram [vs, fs]
  Shaders p
          <$> get (uniformLocation p "fade_factor")
          <*> mapM (get . uniformLocation p) ["textures[0]", "textures[1]"]
          <*> get (attribLocation p "position")

makeResources :: IO Resources
makeResources = Resources
                <$> makeBuffer ArrayBuffer vertexBufferData
                <*> makeBuffer ElementArrayBuffer [0..3::GLuint]
                <*> mapM (makeTexture . ("images" </>)) ["hello1.tga", "hello2.tga"]
                <*> initShaders
                <*> pure 0.0

setupTexturing :: Resources -> IO ()
setupTexturing r = let [t1, t2] = textures r
                       [tu1, tu2] = texturesU (shaders r)
                   in do activeTexture $= TextureUnit 0
                         textureBinding Texture2D $= Just t1
                         uniform tu1 $= Index1 (0 :: GLint)
                         activeTexture $= TextureUnit 1
                         textureBinding Texture2D $= Just t2
                         uniform tu2 $= Index1 (1 :: GLint)

setupGeometry :: Resources -> IO ()
setupGeometry r = let posn = positionA (shaders r)
                      stride = fromIntegral $ sizeOf (undefined :: GLfloat) * 2
                      vad = VertexArrayDescriptor 2 Float stride offset0
                  in do bindBuffer ArrayBuffer $= Just (vertexBuffer r)
                        vertexAttribPointer posn $= (ToFloat, vad)
                        vertexAttribArray posn $= Enabled
                        bindBuffer ElementArrayBuffer $= Just (elementBuffer r)

drawInit :: Resources -> IO ()
drawInit r = do clearColor $= Color4 1 1 1 1
                clear [ ColorBuffer ]
                currentProgram $= Just (getProgram (shaders r))
                setupTexturing r
                setupGeometry r

draw :: Resources -> IO ()
draw r = do uniform (fadeFactorU (shaders r)) $= Index1 (fadeFactor r)
            drawElements TriangleStrip 4 UnsignedInt offset0

animate :: Resources -> IO Resources
animate r = do Just seconds <- getTime
               let fade = sin seconds * 0.5 + 0.5
               return r { fadeFactor = realToFrac fade }

main = do
  ok <- init
  when (not ok) (error "Error Initializing GLFW!")
  windowHint $ WindowHint'RefreshRate 100
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
                keyIsPressed Key'Escape >>= flip unless (animate r >>= go)
  go r0
