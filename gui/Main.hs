import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Linear.V2

import Engine.Data.Types
import Engine.System.Update

-- | Configuration for the simulation
simulationConfig :: Config
simulationConfig = Config
  { gravity = V2 0 (-9.8)
  , timeStep = 1.0 / 60
  , screenBounds = (600, 600)
  , dampeningFactor = 1.0
  }

-- | Render the balls
renderBall :: Ball -> Picture
renderBall ball = translate (realToFrac x) (realToFrac y) $ color ballColor $ circleSolid $ realToFrac (radius ball)
  where
    (V2 x y) = position ball
    ballColor = makeColor 0.2 0.8 0.9 1.0

render :: (Double, [Ball]) -> Picture
render (_, balls) = pictures $ map renderBall balls

-- | Handle input to add new balls
handleInput :: Event -> (Double, [Ball]) -> (Double, [Ball])
handleInput (EventKey (MouseButton LeftButton) Down _ (mx, my)) (dE, balls) =
  let newBall = Ball (V2 (realToFrac mx) (realToFrac my)) (V2 0 0) 15
  in (dE, newBall : balls)
handleInput _ s = s

-- | Wrapper to pass `Config` to `updateWorld`
updateWorldWithConfig :: Float -> (Double, [Ball]) -> (Double, [Ball])
updateWorldWithConfig _ = updateWorld simulationConfig

-- | Returns and array of initial balls, a diagonal line of balls on the screen
initialDiagBalls :: Config -> [Ball]
initialDiagBalls config =
  let (w, h) = screenBounds config
      ballRadius = 10
      ballSpacing = 5
      ballVelocity = V2 0 0
      ballPositions = [ V2 ( x - 300) ( x - 300) | x <- [0, ballRadius + ballSpacing .. w] ]
  in map (\pos -> Ball pos ballVelocity ballRadius) ballPositions


-- | Main entry point
main :: IO ()
main = do
  let initialBalls = [] --initialDiagBalls simulationConfig
      (w, h) = screenBounds simulationConfig -- Extract screen dimensions from Config
  play
    (InWindow "Physics Engine" (round w, round h) (100, 100)) -- Window
    white           -- Background color
    60              -- Frames per second
    (0.0, initialBalls)    -- Initial state
    render          -- Render function
    handleInput     -- Input handling
    updateWorldWithConfig -- Update function with Config
