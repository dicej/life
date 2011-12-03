import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (fromJust, Maybe(Just, Nothing), isNothing)
import Data.List (findIndex)
import Data.IORef (newIORef)
import Graphics.Rendering.OpenGL (vertex, renderPrimitive, flush, ($=!), ($=), 
                                  get, color, GLfloat,
                                  PrimitiveMode(Quads),
                                  Vertex2(Vertex2),
                                  Vertex3(Vertex3),
                                  Color3(Color3),
                                  ClearBuffer(ColorBuffer))
import Graphics.UI.GLUT (clear, postRedisplay, addTimerCallback, 
                         getArgsAndInitialize, initialDisplayMode, 
                         createWindow, displayCallback, mainLoop, swapBuffers,
                         DisplayMode(DoubleBuffered))
import Debug.Trace (trace)

updateIntervalInMillis = 1000

screenWidth = 10 :: GLfloat
screenHeight = 10 :: GLfloat

cellWidth = (7 / (screenWidth * 8))
cellHeight = (7 / (screenHeight * 8))

data World = World { worldMap :: M.Map Int (S.Set Int) }

worldEmpty = World { worldMap = M.empty }

worldSet world (x, y) = World 
  { worldMap = M.insert x (case M.lookup x (worldMap world) of
                              Nothing -> (S.singleton y)
                              Just set -> (S.insert y set)) (worldMap world) }

worldCells world =
  M.foldlWithKey (\ acc x set -> S.fold (\ y acc -> (x, y) : acc) acc set) 
    [] (worldMap world)

-- todo:
worldNext world = world

split index string = (take index string, drop (index + 1) string)

parseCoordinate string =
  let (first, second) = 
        split (fromJust (findIndex (\ c -> c == ',') string)) string in
    (read first, read second)

cellQuad x y =
  let scaledX = ((fromIntegral x) / screenWidth) - (cellWidth / 2.0)
      scaledY = ((fromIntegral y) / screenHeight) - (cellHeight / 2.0) in do
    vertex (Vertex2 scaledX scaledY)
    vertex (Vertex2 scaledX (scaledY + cellHeight))
    vertex (Vertex2 (scaledX + cellWidth) (scaledY + cellHeight))
    vertex (Vertex2 (scaledX + cellWidth) scaledY)

worldDisplay world = do 
  clear [ColorBuffer]
  myWorld <- (get world)
  renderPrimitive Quads (mapM_ (\ (x, y) -> cellQuad x y) (worldCells myWorld))
  swapBuffers

worldUpdate world = do
  myWorld <- (get world)
  world $=! worldNext myWorld
  postRedisplay Nothing
  addTimerCallback updateIntervalInMillis (worldUpdate world)

main = do 
  (_, arguments) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "life"
  world <- newIORef (foldl worldSet worldEmpty (map parseCoordinate arguments))
  displayCallback $= (worldDisplay world)
  addTimerCallback updateIntervalInMillis (worldUpdate world)
  mainLoop
