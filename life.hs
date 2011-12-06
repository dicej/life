import qualified Data.Set as S

import Data.Maybe (fromJust, Maybe(Just, Nothing), isNothing)
import Data.List (findIndex)
import Data.IORef (newIORef)
import Graphics.Rendering.OpenGL (vertex, renderPrimitive, flush, ($=!), ($=), 
                                  get, color, GLfloat, GLsizei,
                                  PrimitiveMode(Quads),
                                  Vertex2(Vertex2),
                                  Vertex3(Vertex3),
                                  Color3(Color3),
                                  ClearBuffer(ColorBuffer))
import Graphics.UI.GLUT (clear, postRedisplay, addTimerCallback, 
                         getArgsAndInitialize, initialDisplayMode,
                         initialWindowSize, createWindow, displayCallback, 
                         mainLoop, swapBuffers,
                         Size(Size),
                         DisplayMode(DoubleBuffered))
import Debug.Trace (trace)

updateIntervalInMillis = 500

windowWidth = 600 :: GLsizei
windowHeight = 600 :: GLsizei

logicalWidth = ((fromIntegral windowWidth) / 10) :: GLfloat
logicalHeight = ((fromIntegral windowHeight) / 10) :: GLfloat

cellWidth = (7 / (logicalWidth * 8))
cellHeight = (7 / (logicalHeight * 8))

data Cell = Cell { cellX :: Int, cellY :: Int }

makeCell x y = Cell { cellX = x, cellY = y }

instance Eq Cell where
  a == b = ((cellX a) == (cellX b)) && ((cellY a) == (cellY b))

instance Ord Cell where
  a <= b = if (cellX a) == (cellX b)
    then (cellY a) <= (cellY b) else (cellX a) < (cellX b)

data World = World { worldLiveCells :: S.Set Cell }

instance Eq World where
  a == b = (worldLiveCells a) == (worldLiveCells b)

cellNeighbors cell = 
  [ makeCell ((cellX cell) - 1) ((cellY cell) - 1),
    makeCell ((cellX cell)    ) ((cellY cell) - 1),
    makeCell ((cellX cell) + 1) ((cellY cell) - 1),
    makeCell ((cellX cell) + 1) ((cellY cell)    ),
    makeCell ((cellX cell) + 1) ((cellY cell) + 1),
    makeCell ((cellX cell)    ) ((cellY cell) + 1),
    makeCell ((cellX cell) - 1) ((cellY cell) + 1),
    makeCell ((cellX cell) - 1) ((cellY cell)    ) ]

cellLive cell world = S.member cell (worldLiveCells world)

cellLiveNeighborCount cell world = foldl 
  (\ sum cell -> if (cellLive cell world) then sum + 1 else sum) 0 
  (cellNeighbors cell)
  
neighborRetain 2 = True
neighborRetain 3 = True
neighborRetain _ = False

neighborAdd 3 = True
neighborAdd _ = False

cellLiveNext cell world =
  (if (cellLive cell world) then neighborRetain else neighborAdd) 
    (cellLiveNeighborCount cell world)

cellVisit cell world acc = World 
  { worldLiveCells = 
      (if (cellLiveNext cell world) then S.insert else S.delete) 
        cell (worldLiveCells acc) }

worldVisit cell world acc = foldl (\ acc cell -> cellVisit cell world acc) acc
  (cell : (cellNeighbors cell))

worldNext world =
  S.fold (\ cell acc -> worldVisit cell world acc) world (worldLiveCells world)

emptyWorld = World { worldLiveCells = S.empty }

worldAddLiveCell world cell =
  world { worldLiveCells = S.insert cell (worldLiveCells world) }

worldAddLiveCells world cells = foldl worldAddLiveCell world cells

split index string = (take index string, drop (index + 1) string)

parseCell string =
  let (first, second) = 
        split (fromJust (findIndex (\ c -> c == ',') string)) string in
    makeCell (read first) (read second)

cellQuad cell =
  let scaledX = ((fromIntegral (cellX cell)) / logicalWidth) 
        - (cellWidth / 2.0)
      scaledY = ((fromIntegral (cellY cell)) / logicalHeight) 
        - (cellHeight / 2.0) in do
    vertex (Vertex2 scaledX scaledY)
    vertex (Vertex2 scaledX (scaledY + cellHeight))
    vertex (Vertex2 (scaledX + cellWidth) (scaledY + cellHeight))
    vertex (Vertex2 (scaledX + cellWidth) scaledY)

worldDisplay world = do 
  clear [ColorBuffer]
  myWorld <- (get world)
  renderPrimitive Quads 
    (mapM_ (\ cell -> cellQuad cell) (S.toList (worldLiveCells myWorld)))
  swapBuffers

worldUpdate world = do
  myWorld <- (get world)
  world $=! worldNext myWorld
  postRedisplay Nothing
  addTimerCallback updateIntervalInMillis (worldUpdate world)

assert test False = putStrLn (test ++ " failed!")
assert test True = putStrLn (test ++ " passed!")

runTests = do
  assert "empty" ((worldNext emptyWorld) == emptyWorld)
  
  assert "no neighbors"
    ((worldNext (worldAddLiveCell emptyWorld (makeCell 0 0))) == emptyWorld)

  let original = worldAddLiveCells emptyWorld [(makeCell 0 0),
                                               (makeCell 0 1),
                                               (makeCell 1 0)] in
    assert "two neighbors"
      ((worldNext original) == (worldAddLiveCell original (makeCell 1 1)))

  let original = worldAddLiveCells emptyWorld [(makeCell 0 0),
                                               (makeCell 0 1),
                                               (makeCell 1 0),
                                               (makeCell 1 1)] in
    assert "three neighbors" ((worldNext original) == original)

main = do
  runTests
  (_, arguments) <- getArgsAndInitialize
  initialWindowSize $= Size windowWidth windowHeight
  initialDisplayMode $= [DoubleBuffered]
  createWindow "life"
  world <- newIORef (worldAddLiveCells emptyWorld (map parseCell arguments))
  displayCallback $= (worldDisplay world)
  addTimerCallback updateIntervalInMillis (worldUpdate world)
  mainLoop
