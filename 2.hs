import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do
  (progName, _) <- getArgsAndInitialize
  createAWindow "Anoop"
  mainLoop

createAWindow windowName = do
  initialDisplayMode $= [ WithSamplesPerPixel 64 ]
  createWindow windowName
  windowPosition $= Position 200 200
  windowSize $= Size 600 600
  displayCallback $= displayPoints

displayPoints = do
  clear [ ColorBuffer ]
  scale 0.8 0.8 (0.8 :: GLfloat)
  translate $ Vector3 (0.3) 0.3 (0 :: GLfloat)
  rotate 30 $ Vector3 0 1 (0 :: GLfloat)
  renderObject Solid$ Teapot 0.6
  flush

