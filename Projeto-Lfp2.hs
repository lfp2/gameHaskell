module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameState = Level Int Bool | GameOver

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
      ball    = objectGroup "ballGroup" [createBall, createBall2, createBall3]
      gameAttribute = GA initScore1 initScore2
      gameMap = colorMap 1.0 0.9 0.8 250 250
      bindings = [(SpecialKey KeyRight, StillDown, moveBar1ToRight),(SpecialKey KeyLeft,  StillDown, moveBar1ToLeft),(Char 'a',  StillDown, moveBar2ToLeft),(Char 'd',  StillDown, moveBar2ToRight),(Char 'q', Press, pressQ)]
      startLevel = Level 0 True
  in funInit winConfig gameMap [bar1,bar2,ball] startLevel gameAttribute bindings gameCycle (Timer 20) []


createBall :: GameObject ()
createBall =
  let ballPic = Basic (Circle 6.0 1.0 0.0 0.0 Filled)
  in object "ball" ballPic False (w-10,h-10) (0,0) ()
  
createBall2 :: GameObject ()
createBall2 =
  let ballPic = Basic (Circle 6.0 0.0 1.0 0.0 Filled)
  in object "ball2" ballPic False (w-10,h-10) (0,0) ()
  
createBall3 :: GameObject ()
createBall3 =
  let ballPic = Basic (Circle 6.0 0.0 0.0 1.0 Filled)
  in object "ball3" ballPic False (w-10,h-10) (0,0) ()


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

moveBar1ToRight :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
moveBar1ToRight _ _ = do
  obj     <- findObject "bar1" "bar1Group"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveBar1ToLeft :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
moveBar1ToLeft _ _ = do
  obj <- findObject "bar1" "bar1Group"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
    then (setObjectPosition ((pX - 5),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)

moveBar2ToLeft :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
moveBar2ToLeft _ _ = do
  obj <- findObject "bar2" "bar2Group"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
    then (setObjectPosition ((pX - 5),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)

moveBar2ToRight :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
moveBar2ToRight _ _ = do
  obj     <- findObject "bar2" "bar2Group"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

gameCycle :: IOGame GameAttribute () GameState () ()
gameCycle = do
  level <- getGameState
  case (level) of
   GameOver -> gameOver
   Level 0 True -> level0
   Level 1 True -> level1
   Level 2 True -> level2
   Level 3 True -> level3
   Level _ False -> keepLevel
  (GA n1 n2) <- getGameAttribute
  printOnScreen (show n1) TimesRoman24 (0,0) 0.0 0.0 0.0
  printOnScreen (show n2) TimesRoman24 (0,h-20) 0.0 0.0 0.0
  ball <- findObject "ball" "ballGroup"
  ball2 <- findObject "ball2" "ballGroup"
  ball3 <- findObject "ball3" "ballGroup"
  ballCollisions ball
  ballCollisions ball2
  ballCollisions ball3
  
  
ballCollisions ball = do
  (GA n1 n2) <- getGameAttribute
  bar01 <- findObject "bar1" "bar1Group"
  bar02 <- findObject "bar2" "bar2Group"
  col1 <- objectLeftMapCollision ball
  col2 <- objectRightMapCollision ball
  when (col1 || col2) (reverseXSpeed ball)
  col3 <- objectTopMapCollision ball
  when col3 (do setObjectPosition (w/2,h/2) ball;
                setGameAttribute(GA (n1 + 1) n2);
                (Level i boo) <- getGameState;
                if (n1 + n2 >= 30) then setGameState (GameOver)
				else if (n1 + n2 == 20) then setGameState (Level 3 True)
				else if (n1 + n2 == 5) then setGameState (Level 2 True)
				else setGameState (Level i boo);)
  col4 <- objectBottomMapCollision ball
  when col4 (do setObjectPosition (w/2,h/2) ball;
                setGameAttribute(GA n1 (n2 + 1));
                (Level i boo) <- getGameState;
                if (n1 + n2 >= 30) then setGameState (GameOver)
				else if (n1 + n2 == 20) then setGameState (Level 3 True)
				else if (n1 + n2 == 5) then setGameState (Level 2 True)
				else setGameState (Level i boo);)
  col5 <- objectsCollision ball bar01
  let (_,vy) = getGameObjectSpeed ball
  when (and [col5, vy < 0])  (reverseYSpeed ball)
  col6 <- objectsCollision ball bar02
  let (_,vy) = getGameObjectSpeed ball
  when (and [col6, vy > 0])  (reverseYSpeed ball)
   
gameOver :: IOGame GameAttribute () GameState () ()
gameOver = do
 (GA n1 n2) <- getGameAttribute
 if (n1 > n2) then printOnScreen ("Jogador 1 venceu") TimesRoman10 (10, h/2) 0.0 0.0 0.0
 else printOnScreen ("Jogador 2 venceu") TimesRoman10 (10, h/2) 0.0 0.0 0.0
 printOnScreen ("Aperte Q para recomecar") TimesRoman10 (10, h/2 - 24) 0.0 0.0 0.0
 ball <- findObject "ball" "ballGroup"
 ball2 <- findObject "ball2" "ballGroup"
 ball3 <- findObject "ball3" "ballGroup"
 setObjectSpeed (0, 0) ball
 setObjectSpeed (0, 0) ball2
 setObjectSpeed (0, 0) ball3

level0 :: IOGame GameAttribute () GameState () ()
level0 = do
 printOnScreen ("Aperte Q para comecar") TimesRoman10 (10, h/2) 0.0 0.0 0.0
 
level1 :: IOGame GameAttribute () GameState () ()
level1 = do
 ball <- findObject "ball" "ballGroup"
 setObjectPosition (w/2,h/2) ball
 setObjectSpeed (3,3) ball
 setGameState (Level 1 False)

level2 :: IOGame GameAttribute () GameState () ()
level2 = do
 ball <- findObject "ball2" "ballGroup"
 setObjectPosition (w/2,h/2) ball
 setObjectSpeed (-3,4) ball
 setGameState (Level 2 False)
 
level3 :: IOGame GameAttribute () GameState () ()
level3 = do
 ball <- findObject "ball3" "ballGroup"
 setObjectPosition (w/2,h/2) ball
 setObjectSpeed (4,-3) ball
 setGameState (Level 3 False)

keepLevel :: IOGame GameAttribute () GameState () ()
keepLevel = do
 (GA n1 n2) <- getGameAttribute
 setGameAttribute (GA n1 n2)
 
pressQ :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
pressQ _ _ = do
 setGameState (Level 1 True)
 setGameAttribute (GA 0 0)