import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do
  (progName, _) <- getArgsAndInitialize
  createAWindow "Anoop"
  mainLoop

createAWindow windowName = do
  createWindow windowName
  windowPosition $= Position 200 200
  windowSize $= Size 600 600
  displayCallback $= displayPoints

displayPoints = do
  clearColor $= Color4 1 1 1 1
  clear [ ColorBuffer ]
  renderPrimitive Polygon myPoints
  flush


myPoints = do
   currentColor $= Color4 1 0 0 1
   vertex$Vertex3 (-0.5) (-0.5) (0.0 :: GLfloat)
   currentColor $= Color4 0 1 0 1
   vertex$Vertex3 (0.5) (-0.5) (0.0 :: GLfloat)
   currentColor $= Color4 0 0 1 1
   vertex$Vertex3 (-0.5) (0.5) (0.0 :: GLfloat)

circle :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
circle radius = circlePoints radius 100

circlePoints radius number
  = [ let alpha = twoPi * i / number
      in (radius * (sin (alpha)), radius * (cos (alpha)), radius * (sin 0))
      | i <- [1, 2 .. number]]
    where 
      twoPi = 2 * pi
