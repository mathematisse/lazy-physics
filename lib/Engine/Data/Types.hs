module Engine.Data.Types (
  Position,
  Velocity,
  Ball(..),
  Config(..)
) where

import Linear.V2

-- | Type definitions
type Position = V2 Double
type Velocity = V2 Double

-- | Ball type representing a single object in the simulation
data Ball = Ball
  { position :: Position
  , velocity :: Velocity
  , radius   :: Double
  } deriving (Show, Eq)

-- | Config object to encapsulate constants and simulation settings
data Config = Config
  { gravity         :: V2 Double    -- Gravitational force
  , timeStep        :: Double       -- Time step for simulation
  , screenBounds    :: (Double, Double) -- Screen width and height
  , dampeningFactor :: Double       -- Velocity reduction on collision
  } deriving (Show, Eq)
