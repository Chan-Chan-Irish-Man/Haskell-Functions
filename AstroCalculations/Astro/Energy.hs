module Astro.Energy where
  import Control.Monad (guard)
  import Astro.Constants
  import Astro.Synonyms

  -- Wien's Displacement Law

  calculatePeakWavelength :: Temperature -> Maybe Wavelength
  calculatePeakWavelength blackBodyTemperature = do
    guard (blackBodyTemperature > 0)

    return (blackBodyTemperature / wienDisplacementConstant)

  -- Relativistic Doppler Shift

  calculateRelativisticDopplerShift :: Wavelength -> Velocity -> Maybe Wavelength
  calculateRelativisticDopplerShift sourceWavelength wavelengthRelativeVelocity = do
    guard (wavelengthRelativeVelocity <= speedOfLight)

    return (sourceWavelength * sqrt((1 + wavelengthRelativeVelocity / speedOfLight) / 
                                    (1 - wavelengthRelativeVelocity / speedOfLight)))