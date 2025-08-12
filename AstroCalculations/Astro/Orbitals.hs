module Astro.Orbitals where
  import Control.Monad (guard)
  import Astro.Constants
  import Astro.Synonyms

  -- Newton's Law of Universal Gravitation

  calculateUniversalGravitation firstObjectMass secondObjectMass distanceBetweenCenterOfBothObjects = do
    guard (firstObjectMass > 0)
    guard (secondObjectMass > 0)
    guard (distanceBetweenCenterOfBothObjects > 0)

    return (gravitationalConstant * ((firstObjectMass * secondObjectMass) / distanceBetweenCenterOfBothObjects^2))

  -- Calculate Orbital Period

  calculateOrbitalPeriod :: Distance -> Mass -> Mass -> Maybe Time
  calculateOrbitalPeriod semiMajorAxis centralMass orbitingMass = do
    guard (semiMajorAxis > 0)
    guard (centralMass > 0)
    guard (orbitingMass > 0)

    return (sqrt((4 * pi^2 * semiMajorAxis^3) / (gravitationalConstant * (centralMass + orbitingMass))))

  -- Calculate Orbital Velocity

  calculateOrbitalVelocity :: Distance -> Time -> Maybe Velocity
  calculateOrbitalVelocity semiMajorAxis orbitalPeriod = do
    guard (semiMajorAxis > 0)
    guard (orbitalPeriod > 0)

    return ((2 * pi * semiMajorAxis) / orbitalPeriod)

  -- Escape Velocity

  calculateEscapeVelocity :: Mass -> Distance -> Maybe Velocity
  calculateEscapeVelocity objectMass distanceFromObjectCenter = do
    guard (objectMass > 0)
    guard (distanceFromObjectCenter > 0)

    return (sqrt((2 * gravitationalConstant * objectMass) / distanceFromObjectCenter))

  -- Tidal Force

  calculateTidalForce :: Mass -> Mass -> Distance -> Distance -> Maybe Force
  calculateTidalForce largerObjectMass smallerObjectMass
                      differenceBetweenTwoPointsOnSmallerObject
                      distanceBetweenCenterOfBothObjects = do
    guard (largerObjectMass > 0)
    guard (smallerObjectMass > 0)
    guard (differenceBetweenTwoPointsOnSmallerObject > 0)
    guard (distanceBetweenCenterOfBothObjects > 0)
    
    return ((2 * gravitationalConstant * largerObjectMass *
                smallerObjectMass * differenceBetweenTwoPointsOnSmallerObject)
                / distanceBetweenCenterOfBothObjects^3)

  -- Habitable Zone

  calculateHabitableZone :: Luminosity -> Maybe (Distance, Distance)
  calculateHabitableZone solarLuminosity = do
    guard (solarLuminosity > 0)

    innerHabitableZone <- calculateInnerHabitableZone solarLuminosity
    outerHabitableZone <- calculateOuterHabitableZone solarLuminosity

    return (innerHabitableZone, outerHabitableZone)

  calculateInnerHabitableZone :: Luminosity -> Maybe Distance
  calculateInnerHabitableZone solarLuminosity = do
    guard (solarLuminosity > 0)

    return (sqrt(solarLuminosity/innerStellarFlux) * au)

  calculateOuterHabitableZone :: Luminosity -> Maybe Distance
  calculateOuterHabitableZone solarLuminosity = do
    guard (solarLuminosity > 0)

    return (sqrt(solarLuminosity/outerStellarFlux) * au)

  -- Gravitational Time Dilation

  calculateGravitationalTimeDilationFactor :: Mass -> Distance -> Maybe Factor
  calculateGravitationalTimeDilationFactor mass distanceFromObjectCenter = do
    guard (mass > 0)
    guard (distanceFromObjectCenter > 0)
    let schwartzschildRadius = (2 * gravitationalConstant * mass) / speedOfLight^2
    guard (distanceFromObjectCenter > schwartzschildRadius)

    return (sqrt(1 - (2 * gravitationalConstant * mass) / (distanceFromObjectCenter * speedOfLight^2)))

  calculateDilatedTime :: Time -> Mass -> Distance -> Maybe Time
  calculateDilatedTime timeInterval mass distanceFromObjectCenter = do
    guard (timeInterval > 0)
    factor <- calculateGravitationalTimeDilationFactor mass distanceFromObjectCenter
    
    return (timeInterval * factor)

  -- Lorentz Factor

  calculateLorentzFactor :: Velocity -> Maybe Factor
  calculateLorentzFactor speedOfMovingObserver = do
    guard (speedOfMovingObserver > 0)
    guard (speedOfMovingObserver <= speedOfLight)

    return (1 / sqrt(1 - (speedOfMovingObserver^2/speedOfLight^2)))

  -- Roche Limit

  calculateRocheLimit :: Distance -> Density -> Density -> Maybe Distance
  calculateRocheLimit primaryBodyRadius primaryBodyDensity satelliteDensity = do
    guard (primaryBodyRadius > 0)
    guard (primaryBodyDensity > 0)
    guard (satelliteDensity > 0)

    return (primaryBodyRadius * ((2 * primaryBodyDensity) / satelliteDensity) ** (1/3))

  -- Vis-Viva Equation

  calculateVisaViva :: Mass -> Distance -> Distance -> Maybe Velocity
  calculateVisaViva centralMass distanceFromCenterOfCentralMassAndOrbitingBody semiMajorAxis = do
    guard (centralMass > 0)
    guard (distanceFromCenterOfCentralMassAndOrbitingBody > 0)
    guard (semiMajorAxis > 0)

    return (sqrt(gravitationalConstant * centralMass * 
          (2 / distanceFromCenterOfCentralMassAndOrbitingBody -
          1 / semiMajorAxis)))
