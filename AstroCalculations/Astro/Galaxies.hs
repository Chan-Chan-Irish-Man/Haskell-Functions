module Astro.Galaxies where
  import Control.Monad (guard)
  import Constants
  import Synonyms

  estimateGalaxyMassVirial :: Velocity -> Distance -> Maybe Mass
  estimateGalaxyMassVirial stellarVelocityDispersion radiusLightYears = do
    guard(stellarVelocityDispersion > 0)
    guard(radiusLightYears > 0)

    let radiusMeters = lightYearsToMeters radiusLightYears
        result = (3 * stellarVelocityDispersion^2 * radiusMeters) / gravitationalConstant
    return result

  -- Hubble Expansion

  calculateHubbleExpansion :: Distance -> Maybe Velocity
  calculateHubbleExpansion properDistance = do
    guard(properDistance > 0)

    return (hubblesConstant * properDistance)