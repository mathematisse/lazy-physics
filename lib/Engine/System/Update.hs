module Engine.System.Update (
  updateWorld,
  updateBall,
  handleConstraints
) where

import Linear.V2
import Linear.Vector
import Linear.Metric
import Control.Lens

import Engine.Data.Types

import Debug.Trace

-- | Update the ball's state using Velocity Verlet integration
updateBallState :: Config -> Ball -> Ball
updateBallState config ball =
  let dt = timeStep config
      g = gravity config
      oldPos = position ball
      oldVel = velocity ball
      -- Update position based on current velocity and acceleration
      newPos = oldPos + (dt *^ oldVel) + (0.5 * (dt ** 2) *^ g)
      -- Calculate new velocity
      newVel = oldVel + (dt *^ g)
  in ball { position = newPos, velocity = newVel }




-- | Handles collisions with screen boundaries, flipping and damping the velocity if necessary
handleConstraints :: Config -> Ball -> Ball
handleConstraints config ball =
  let
      -- Constants
      V2 x y = position ball
      V2 vX vY = velocity ball
      (width, height) = screenBounds config
      r = radius ball
      g = norm (gravity config) -- Magnitude of gravity

      -- Boundary limits
      bottom = -height / 2 + r
      top = height / 2 - r
      left = -width / 2 + r
      right = width / 2 - r

      -- Correct for being below the bottom boundary
      (adjustedYBottom, correctedVYBottom) =
        if y < bottom
        then
          let deltaY = bottom - y
              lostPotentialEnergy = g * deltaY
              correctedVelocitySquared = max (vY ** 2 - 2 * lostPotentialEnergy) 0
              reflectedVelocity = sqrt correctedVelocitySquared
          in (bottom, reflectedVelocity) -- Reflected upward
        else (y, vY)

      -- Correct for being above the top boundary
      (adjustedYTop, correctedVYTop) =
        if adjustedYBottom > top
        then
          let deltaY = adjustedYBottom - top
              lostPotentialEnergy = g * deltaY
              correctedVelocitySquared = max (correctedVYBottom ** 2 - 2 * lostPotentialEnergy) 0
              reflectedVelocity = -sqrt correctedVelocitySquared
          in (top, reflectedVelocity) -- Reflected downward
        else (adjustedYBottom, correctedVYBottom)

      -- Correct for horizontal boundaries
      adjustedX = if x < left then left else if x > right then right else x
      finalVX = if (x < left && vX < 0) || (x > right && vX > 0)
                then -vX
                else vX

  in ball { position = V2 adjustedX adjustedYTop, velocity = V2 finalVX correctedVYTop }





-- | Checks if two balls are colliding
areColliding :: Ball -> Ball -> Bool
areColliding b1 b2 =
  let d = distance (position b1) (position b2)
  in d <= (radius b1 + radius b2)

-- | Resolves the collision between two balls and returns their updated velocities
resolveCollision :: Ball -> Ball -> (Ball, Ball)
resolveCollision b1 b2 =
  let p1 = position b1
      p2 = position b2
      v1 = velocity b1
      v2 = velocity b2
      r1 = radius b1
      r2 = radius b2
      m1 = r1 ** 2  -- Assuming mass is proportional to the square of the radius
      m2 = r2 ** 2
      distanceBetweenCenters = norm (p2 - p1)
      normal@(V2 normX normY) = normalize (p2 - p1)
      tangent = V2 (- (normY)) (normX)
      v1n = dot v1 normal
      v1t = dot v1 tangent
      v2n = dot v2 normal
      v2t = dot v2 tangent
      v1n' = (v1n * (m1 - m2) + 2 * m2 * v2n) / (m1 + m2)
      v2n' = (v2n * (m2 - m1) + 2 * m1 * v1n) / (m1 + m2)
      v1nVec' = v1n' *^ normal
      v1tVec = v1t *^ tangent
      v2nVec' = v2n' *^ normal
      v2tVec = v2t *^ tangent
      newVel1 = v1nVec' + v1tVec
      newVel2 = v2nVec' + v2tVec
      overlap = (r1 + r2) - distanceBetweenCenters
      correction = (overlap / 2) *^ normal
      newPos1 = p1 - correction
      newPos2 = p2 + correction
  in (b1 { position = newPos1, velocity = newVel1 },
      b2 { position = newPos2, velocity = newVel2 })







-- | Compute the kinetic energy of a ball
kineticEnergy :: Ball -> Double
kineticEnergy ball =
  0.5 * (radius ball ** 2) * quadrance (velocity ball) -- K.E. = 1/2 m v^2

-- | Compute the potential energy of a ball
potentialEnergy :: Config -> Ball -> Double
potentialEnergy config ball =
  let m = radius ball ** 2
      g = norm (gravity config) -- Magnitude of gravitational acceleration
      V2 _ y = position ball
      h = y + (snd (screenBounds config) / 2)-- Height (y-coordinate)
  in m * g * h -- P.E. = m g h

-- | Compute the total energy (kinetic + potential)
totalEnergy :: Config -> [Ball] -> Double
totalEnergy config balls =
  sum (map kineticEnergy balls) + sum (map (potentialEnergy config) balls)




-- | Updates a ball's position, velocity, and handles collisions
updateBall :: Config -> Ball -> Ball
updateBall config = handleConstraints config . updateBallState config

-- | Resolves all collisions in the system
resolveCollisions :: [Ball] -> [Ball]
resolveCollisions balls =
  foldl resolvePair balls [(b1, b2) | b1 <- balls, b2 <- balls, b1 /= b2, areColliding b1 b2]
  where
    resolvePair :: [Ball] -> (Ball, Ball) -> [Ball]
    resolvePair currentBalls (b1, b2) =
      let (updatedB1, updatedB2) = resolveCollision b1 b2
      in map (\b -> if b == b1 then updatedB1 else if b == b2 then updatedB2 else b) currentBalls


-- | Updates the world state, including ball-to-ball collisions
updateWorld :: Config -> (Double, [Ball]) -> (Double, [Ball])
updateWorld config (totDE, balls) =
  let updatedBalls = map (updateBall config) balls
      updatedBalls' = resolveCollisions updatedBalls
      deltaE = (totalEnergy config updatedBalls') - (totalEnergy config balls)
      -- if abs less than 2e-10 per balls, then deltaE is 0
      deltaE' = if abs deltaE < (2e-8 * fromIntegral (length balls)) then 0 else deltaE
      printedTotDE = if abs totDE < (2e-8 * fromIntegral (length balls)) then 0 else totDE
  in trace ("Total delta " ++ (show printedTotDE) ++ " delta E " ++ show deltaE') $ (deltaE + totDE, updatedBalls')

