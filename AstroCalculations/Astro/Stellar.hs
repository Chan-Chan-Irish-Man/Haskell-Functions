module Astro.Stellar where
  import Constants
  import Synonyms

  -- Stefan-Boltzmann Law for Luminosity

  calculateLuminosity :: Distance -> Temperature -> Maybe Luminosity
  calculateLuminosity starRadius starSurfaceTemperature = do
    guard(starRadius > 0)
    guard(starSurfaceTemperature > 0)

    return (4 * pi * starRadius^2 * 
            stefanBoltzmannConstant * starSurfaceTemperature^4)

  calculateLuminosityInSolarUnits :: Distance -> Temperature -> Maybe Luminosity
  calculateLuminosityInSolarUnits starRadius starSurfaceTemperature = do
    guard(starRadius > 0)
    guard(starSurfaceTemperature > 0)

    luminosity <- calculateLuminosity starRadius starSurfaceTemperature
    return (luminosity / solarLuminosity)

  -- Stefan-Boltzmann Law for Radius

  calculateStellarRadius :: Luminosity -> Temperature -> Maybe Distance
  calculateStellarRadius luminosity surfaceTemperature = do
    guard (luminosity > 0)
    guard (surfaceTemperature > 0)

    return (sqrt(luminosity / (4 * pi * stefanBoltzmannConstant * surfaceTemperature^4)))

  -- Star Lifetime

  calculateStarLifetimeInYears :: Mass -> Luminosity -> Maybe Time
  calculateStarLifetimeInYears starMass starLuminosity = do
    guard(starMass > 0)
    guard(starLuminosity > 0)

    return ((starMass / solarMass) / (starLuminosity / solarLuminosity) * solarLifetimeInYears)

  calculateStarLifetimeFromLuminosityInYears :: Luminosity -> Maybe Time 
  calculateStarLifetimeFromLuminosityInYears starLuminosity = do
    guard(starLuminosity > 0)

    starMass <- calculateStellarMassFromLuminosity starLuminosity
    calculateStarLifetimeInYears starMass starLuminosity

  calculateStarLifetimeFromMassInYears :: Mass -> Maybe Time
  calculateStarLifetimeFromMassInYears starMass = do
    guard(starMass > 0)

    starLuminosity <- calculateStellarLuminosityFromMass starMass
    calculateStarLifetimeInYears starMass starLuminosity

  -- Stellar Mass From Luminosity

  calculateStellarMassFromLuminosity :: Luminosity -> Maybe Mass
  calculateStellarMassFromLuminosity luminosity = do
    guard(luminosity > 0)

    return (solarMass * (luminosity / solarLuminosity) ** (1/3.5))

  -- Luminosity from Stellar Mass

  calculateStellarLuminosityFromMass :: Mass -> Maybe Luminosity
  calculateStellarLuminosityFromMass mass = do
    guard(mass > 0)

    return (solarLuminosity * (mass / solarMass) ** 3.5)

  -- Absolute Magnitude from Apparent Magnitude

  calculateAbsoluteMagnitudeFromApparentMagnitude :: Brightness -> Distance -> Maybe Brightness
  calculateAbsoluteMagnitudeFromApparentMagnitude apparentMagnitude distanceInParsecs = do
    guard(distanceInParsecs > 0)
    
    let log10Distance = logBase 10 distanceInParsecs
    
    return (apparentMagnitude - 5 * log10Distance + 5)

  -- Stellar Parallax

  calculateStellarParallax :: Angle -> Maybe Distance
  calculateStellarParallax parallaxAngle = do
    guard(parallaxAngle > 0)

    return (1 / parallaxAngle)