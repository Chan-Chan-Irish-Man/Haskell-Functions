import Control.Monad (guard)

-- Type Synonyms

type Mass = Double
type Distance = Double
type Time = Double
type Luminosity = Double
type Temperature = Double
type Velocity = Double
type Force = Double
type Factor = Double
type Density = Double
type Quantity = Double
type Wavelength = Double
type Unit = Char

-- Constants

secondsInAMinute :: Num a => a
secondsInAMinute = 60

secondsInAnHour :: Num a => a
secondsInAnHour = 3600

secondsInADay :: Num a => a
secondsInADay = 86400

secondsInAWeek :: Num a => a
secondsInAWeek = 60480

secondsInAYear :: Num a => a
secondsInAYear = 31557600

au :: Fractional a => a
au = 1.496e11 -- meters

earthMass :: Fractional a => a
earthMass = 5.972e24 -- kg

solarMass :: Fractional a => a
solarMass = 1.989e30 -- kg

solarSurfaceTemperature :: Num a => a
solarSurfaceTemperature = 5772 -- Kelvin

solarLuminosity :: Fractional a => a
solarLuminosity = 3.828e26 -- Watts

solarRadius :: Fractional a => a
solarRadius = 6.9634e8 -- m

solarLifetimeInYears :: Fractional a => a
solarLifetimeInYears = 1e10

milkyWaySolarMasses :: Fractional a => a
milkyWaySolarMasses = 1.5e12 -- solar masses

gravitationalConstant :: Fractional a => a
gravitationalConstant = 6.674e-11

lightYearInMeters :: Fractional a => a
lightYearInMeters = 9.461e15 -- m

speedOfLight :: Num a => a
speedOfLight = 299792458 -- m/s

stefanBoltzmannConstant :: Fractional a => a
stefanBoltzmannConstant = 5.67e-8 -- W m^-2 K^-4

hubblesConstant :: Fractional a => a
hubblesConstant = 674e2 -- m/s/Mpc (as observed by the Planck satellite)

wienDisplacementConstant :: Fractional a => a
wienDisplacementConstant = 2.89777e-3

innerStellarFlux :: Fractional a => a
innerStellarFlux = 1.1

outerStellarFlux :: Fractional a => a
outerStellarFlux = 0.53

-- Utility functions

lightYearsToMeters :: Distance -> Distance
lightYearsToMeters lightYears =
  lightYears * lightYearInMeters

kgToSolarMass :: Mass -> Mass
kgToSolarMass kg =
  kg / solarMass

solarMassesToMilkyWayMasses :: Mass -> Mass
solarMassesToMilkyWayMasses solarMasses =
  solarMasses / milkyWaySolarMasses

convertSecondsToUnit :: Unit -> Time -> Time
convertSecondsToUnit unit periodInSeconds =
  case unit of
    's' -> periodInSeconds
    'm' -> periodInSeconds / secondsInAMinute
    'h' -> periodInSeconds / secondsInAnHour
    'd' -> periodInSeconds / secondsInADay
    'w' -> periodInSeconds / secondsInAWeek
    'y' -> periodInSeconds / secondsInAYear
    _   -> periodInSeconds

toEarthMasses :: Mass -> Quantity
toEarthMasses mass =
  mass / earthMass

toAU :: Distance -> Quantity
toAU meters =
  meters / au

-- Swarzschild Radius

calculateSchwarzschildRadius :: Mass -> Maybe Distance
calculateSchwarzschildRadius mass = do
  guard(mass > 0)

  return ((2 * gravitationalConstant * mass) / speedOfLight^2)

-- Galaxy mass estimator

estimateGalaxyMassVirial :: Velocity -> Distance -> Maybe Mass
estimateGalaxyMassVirial stellarVelocityDispersion radiusLightYears = do
  guard(stellarVelocityDispersion > 0)
  guard(radiusLightYears > 0)

  let radiusMeters = lightYearsToMeters radiusLightYears
      result = (3 * stellarVelocityDispersion^2 * radiusMeters) / gravitationalConstant
  return result

-- Orbital Calculations

calculateOrbitalPeriod :: Distance -> Mass -> Mass -> Maybe Time
calculateOrbitalPeriod semiMajorAxis centralMass orbitingMass = do
  guard(semiMajorAxis > 0)
  guard(centralMass > 0)
  guard(orbitingMass > 0)

  return (sqrt((4 * pi^2 * semiMajorAxis^3) / (gravitationalConstant * (centralMass + orbitingMass))))

calculateOrbitalVelocity :: Distance -> Time -> Maybe Velocity
calculateOrbitalVelocity semiMajorAxis orbitalPeriod = do
  guard(semiMajorAxis > 0)
  guard(orbitalPeriod > 0)

  return ((2 * pi * semiMajorAxis) / orbitalPeriod)

-- Escape Velocity

calculateEscapeVelocity :: Mass -> Distance -> Maybe Velocity
calculateEscapeVelocity objectMass distanceFromObjectCenter = do
  guard(objectMass > 0)
  guard(distanceFromObjectCenter > 0)

  return (sqrt((2 * gravitationalConstant * objectMass) / distanceFromObjectCenter))

-- Tidal Force

calculateTidalForce :: Mass -> Mass -> Distance -> Distance -> Maybe Force
calculateTidalForce largerObjectMass smallerObjectMass
                    differenceBetweenTwoPointsOnSmallerObject
                    distanceBetweenCenterOfBothObjects = do
  guard(largerObjectMass > 0)
  guard(smallerObjectMass > 0)
  guard(differenceBetweenTwoPointsOnSmallerObject > 0)
  guard(distanceBetweenCenterOfBothObjects > 0)
  
  return ((2 * gravitationalConstant * largerObjectMass *
               smallerObjectMass * differenceBetweenTwoPointsOnSmallerObject)
               / distanceBetweenCenterOfBothObjects^3)

-- Luminosity

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

-- Hubble Expansion

calculateHubbleExpansion :: Distance -> Maybe Velocity
calculateHubbleExpansion properDistance = do
  guard(properDistance > 0)

  return (hubblesConstant * properDistance)

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
  guard(speedOfMovingObserver > 0)
  guard(speedOfMovingObserver <= speedOfLight)

  return (1 / sqrt(1 - (speedOfMovingObserver^2/speedOfLight^2)))

-- Roche Limit

calculateRocheLimit :: Distance -> Density -> Density -> Maybe Distance
calculateRocheLimit primaryBodyRadius primaryBodyDensity satelliteDensity = do
  guard(primaryBodyRadius > 0)
  guard(primaryBodyDensity > 0)
  guard(satelliteDensity > 0)

  return (primaryBodyRadius * ((2 * primaryBodyDensity) / satelliteDensity) ** (1/3))

-- Photon Sphere Radius

calculatePhotonSphereRadius :: Mass -> Maybe Distance
calculatePhotonSphereRadius mass = do
  guard(mass > 0)

  schwarzschildRadius <- calculateSchwarzschildRadius mass

  return (1.5 * schwarzschildRadius)

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
  starLifetimeInYears <- calculateStarLifetimeInYears starMass starLuminosity

  return starLifetimeInYears

calculateStarLifetimeFromMassInYears :: Mass -> Maybe Time
calculateStarLifetimeFromMassInYears starMass = do
  guard(starMass > 0)

  starLuminosity <- calculateStellarLuminosityFromMass starMass
  starLifetime <- calculateStarLifetimeInYears starMass starLuminosity
  
  return starLifetime

-- Habitable Zone

calculateHabitableZone :: Luminosity -> Maybe (Distance, Distance)
calculateHabitableZone solarLuminosity = do
  guard(solarLuminosity > 0)

  innerHabitableZone <- calculateInnerHabitableZone solarLuminosity
  outerHabitableZone <- calculateOuterHabitableZone solarLuminosity

  return (innerHabitableZone, outerHabitableZone)

calculateInnerHabitableZone :: Luminosity -> Maybe Distance
calculateInnerHabitableZone solarLuminosity = do
  guard(solarLuminosity > 0)

  return (sqrt(solarLuminosity/innerStellarFlux) * au)

calculateOuterHabitableZone :: Luminosity -> Maybe Distance
calculateOuterHabitableZone solarLuminosity = do
  guard(solarLuminosity > 0)

  return (sqrt(solarLuminosity/outerStellarFlux) * au)

-- Wien's Displacement Law

calculatePeakWavelength :: Temperature -> Maybe Wavelength
calculatePeakWavelength blackBodyTemperature = do
  guard(blackBodyTemperature > 0)

  return (blackBodyTemperature / wienDisplacementConstant)