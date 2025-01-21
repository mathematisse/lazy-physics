module UpdateSpec (spec) where

import Test.Hspec
import Linear.V2 (V2(..))
import Engine.Data.Types
import Engine.System.Update

-- | Simplified configuration for testing
testConfig :: Config
testConfig = Config
  { gravity = V2 0 (-1.0) -- Simple downward gravity
  , timeStep = 1.0        -- 1-second timestep for easy calculations
  , screenBounds = (100, 100) -- Small screen bounds
  , dampeningFactor = 0.5       -- Damping factor for collisions
  }

-- | Helper function to create a simple ball
mkBall :: V2 Double -> V2 Double -> Double -> Ball
mkBall pos vel rad = Ball { position = pos, velocity = vel, radius = rad }

-- | Test cases
spec :: Spec
spec = do
  describe "applyGravity" $ do
    it "applies gravity to the velocity" $ do
      let ball = mkBall (V2 0 0) (V2 0 0) 10
      let updatedBall = applyGravity testConfig ball
      velocity updatedBall `shouldBe` V2 0 (-1.0)

  describe "updatePosition" $ do
    it "updates position based on velocity" $ do
      let ball = mkBall (V2 0 0) (V2 5 0) 10
      let updatedBall = updatePosition testConfig ball
      position updatedBall `shouldBe` V2 5 0

  describe "handleCollisions" $ do
    it "handles vertical boundary collisions correctly" $ do
      let ball = mkBall (V2 0 (-60)) (V2 0 (-10)) 10
      let updatedBall = handleCollisions testConfig ball
      velocity updatedBall `shouldBe` V2 0 5.0 -- Correctly applies dampening factor

    it "does nothing if no collision occurs" $ do
      let ball = mkBall (V2 0 0) (V2 0 (-10)) 10
      let updatedBall = handleCollisions testConfig ball
      velocity updatedBall `shouldBe` V2 0 (-10)

  describe "updateBall" $ do
    it "applies gravity, updates position, and handles collisions" $ do
      let ball = mkBall (V2 0 (-60)) (V2 0 (-10)) 10
      let updatedBall = updateBall testConfig ball
      position updatedBall `shouldBe` V2 0 (-71) -- Position updated by velocity
      velocity updatedBall `shouldBe` V2 0 5.5   -- Collision flips velocity with 0.5 dampening
