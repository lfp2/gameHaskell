module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameState = LevelStart Int | Level Int | GameOver
data GameAttribute = GA Int Int



width = 250
height = 250
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

initScore1, initScore2 :: Int
initScore1 = 0
initScore2 = 0

main :: IO ()
main =
  let winConfig = ((0,0),(width,height),"Project - Lfp2 Press Q to quit")
      bar1     = objectGroup "bar1Group"  [createBar1]
      bar2     = objectGroup "bar2Group"  [createBar2]
      ball    = objectGroup "ballGroup" [createBall]
      gameAttribute = GA initScore1 initScore2
      gameMap = colorMap 1.0 0.9 0.8 250 250
      bindings = [(SpecialKey KeyRight, StillDown, moveBar1ToRight),(SpecialKey KeyLeft,  StillDown, moveBar1ToLeft),(Char 'a',  StillDown, moveBar2ToLeft),(Char 'd',  StillDown, moveBar2ToRight),(Char 'q', Press, \_ _ -> funExit)]
  in funInit winConfig gameMap [bar1,bar2,ball] () gameAttribute bindings gameCycle Idle []


createBall :: GameObject ()
createBall =
  let ballPic = Basic (Circle 6.0 0.0 1.0 0.0 Filled)
  in object "ball" ballPic False (w/2,h/2) (-3,3) ()


createBar1 :: GameObject ()
createBar1 =
  let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)]
      barPic   = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
  in object "bar1" barPic False (w/2,20) (0,0) ()
  
createBar2 :: GameObject ()
createBar2 =
  let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)]
      barPic   = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
  in object "bar2" barPic False (w/2,230) (0,0) ()

moveBar1ToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBar1ToRight _ _ = do
  obj     <- findObject "bar1" "bar1Group"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveBar1ToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBar1ToLeft _ _ = do
  obj <- findObject "bar1" "bar1Group"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
    then (setObjectPosition ((pX - 5),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)

moveBar2ToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBar2ToLeft _ _ = do
  obj <- findObject "bar2" "bar2Group"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
    then (setObjectPosition ((pX - 5),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)

moveBar2ToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBar2ToRight _ _ = do
  obj     <- findObject "bar2" "bar2Group"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

gameCycle :: IOGame GameAttribute () () () ()
gameCycle = do
  (GA n1 n2) <- getGameAttribute
  printOnScreen (show n1) TimesRoman24 (0,0) 0.0 0.0 0.0
  printOnScreen (show n2) TimesRoman24 (0,h-20) 0.0 0.0 0.0
  ball <- findObject "ball" "ballGroup"
  bar01 <- findObject "bar1" "bar1Group"
  bar02 <- findObject "bar2" "bar2Group"
  col1 <- objectLeftMapCollision ball
  col2 <- objectRightMapCollision ball
  when (col1 || col2) (reverseXSpeed ball)
  col3 <- objectTopMapCollision ball
  when col3 (do setObjectPosition (w/2,h/2) ball
                setGameAttribute(GA (n1 + 1) n2))
  col4 <- objectBottomMapCollision ball
  when col4 (do setObjectPosition (w/2,h/2) ball
                setGameAttribute(GA n1 (n2 + 1)))
  col5 <- objectsCollision ball bar01
  let (_,vy) = getGameObjectSpeed ball
  when (and [col5, vy < 0])  (reverseYSpeed ball)
  col6 <- objectsCollision ball bar02
  let (_,vy) = getGameObjectSpeed ball
  when (and [col6, vy > 0])  (reverseYSpeed ball)
  