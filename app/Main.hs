module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

data PongGame = Game
  { ballLoc :: (Float, Float)
  , ballVel :: (Float, Float)
  , player1 :: Player
  , player2 :: Player
  , paused :: Bool
  } deriving Show

data Player = Player
  { loc :: Float
  , upHeld :: Bool
  , downHeld :: Bool
  } deriving Show

initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (50, -150)
  , player1 = Player { loc = -100, upHeld = False, downHeld = False }
  , player2 = Player { loc = 40, upHeld = False, downHeld = False }
  , paused = False
  }

background :: Color
background = black

render :: PongGame -> Picture
render game = pictures
  [ ball
  , walls
  , mkPaddle rose 120 $ loc $ player1 game
  , mkPaddle orange (-120) $ loc $ player2 game
  ]
    where
      -- The pong ball.
      ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
      ballColor = dark red

      -- The bottom and top walls.
      wall :: Float -> Picture
      wall offset =
        translate 0 offset $
          color wallColor $
            rectangleSolid 270 10

      wallColor = greyN 0.5
      walls = pictures [ wall 150, wall (-150) ]

      -- Make a paddle of a given border and vertical offset.
      mkPaddle :: Color -> Float -> Float -> Picture
      mkPaddle col x y = pictures
        [ translate x y $ color col $ rectangleSolid 26 86
        , translate x y $ color paddleColor $ rectangleSolid 20 80]

      paddleColor = light (light blue)

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations
    x' = x + vx * seconds
    y' = y + vy * seconds

type Radius = Float
type Position = (Float, Float)

paddleBounce :: PongGame -> PongGame
paddleBounce game =
  game { ballVel = (vx', vy) }
    where
      radius = 10

      (x, y) = ballLoc game
      (vx, vy) = ballVel game
      p1 = loc $ player1 game
      p2 = loc $ player2 game

      paddleHeight = 43
      paddleWidth = 13

      player1Collision =
        x + radius >= 120 - paddleWidth
        && y <= p1 + paddleHeight
        && y >= p1 - paddleHeight

      player2Collision =
        x - radius <= -120 + paddleWidth
        && y <= p2 + paddleHeight
        && y >= p2 - paddleHeight

      vx' = if player1Collision || player2Collision
               then -vx
               else vx

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    radius = 10

    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
            then
              -vy
            else
              vy

wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral width / 2
    bottomCollision = y + radius >= fromIntegral width / 2

movePaddles :: PongGame -> PongGame
movePaddles game =
  game
    { player1 = p1 { loc = n1 }
    , player2 = p2 { loc = n2 }
    }
  where
    p1 = player1 game
    p2 = player2 game
    n1 = getNewLoc p1
    n2 = getNewLoc p2
    
    getNewLoc :: Player -> Float
    getNewLoc player =
      if upHeld player
        then min (loc player + 5) 100
      else if downHeld player
        then max (loc player - 5) (-100)
      else loc player



endGame :: PongGame -> PongGame
endGame game =
  if x <= -120
    then error "Player 1 wins"
  else if x >= 120
    then error "Player 2 wins"
  else game
    where
      (x, _) = ballLoc game

handleKeys :: Event -> PongGame -> PongGame

handleKeys (EventKey (Char 'p') Down _ _) game =
  game { paused = not p }
    where p = paused game

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game =
  game { player1 = (player1 game) { upHeld = True } }

handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game =
  game { player1 = (player1 game) { upHeld = False } }

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game =
  game { player1 = (player1 game) { downHeld = True } }

handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game =
  game { player1 = (player1 game) { downHeld = False } }

handleKeys (EventKey (Char 'w') Down _ _) game =
  game { player2 = (player2 game) { upHeld = True } }

handleKeys (EventKey (Char 'w') Up _ _) game =
  game { player2 = (player2 game) { upHeld = False } }

handleKeys (EventKey (Char 's') Down _ _) game =
  game { player2 = (player2 game) { downHeld = True } }

handleKeys (EventKey (Char 's') Up _ _) game =
  game { player2 = (player2 game) { downHeld = False } }

handleKeys _ game = game

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

update :: Float -> PongGame -> PongGame
update seconds game =
  if paused game
     then game
     else endGame $ paddleBounce $ wallBounce $ movePaddles $ moveBall seconds game 
