module Astro.BlackHoles where
  import Control.Monad (guard)
  import Astro.Constants
  import Astro.Synonyms

  -- Calculate Schwarzschild Radius

  calculateSchwarzschildRadius :: Mass -> Maybe Distance
  calculateSchwarzschildRadius mass = do
    guard(mass > 0)

    return ((2 * gravitationalConstant * mass) / speedOfLight^2)

  -- Photon Sphere Radius

  calculatePhotonSphereRadius :: Mass -> Maybe Distance
  calculatePhotonSphereRadius mass = do
    guard(mass > 0)

    schwarzschildRadius <- calculateSchwarzschildRadius mass

    return (1.5 * schwarzschildRadius)