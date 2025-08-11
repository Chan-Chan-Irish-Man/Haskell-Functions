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

convertPeriod :: Unit -> Time -> Time
convertPeriod unit periodInSeconds =
  case unit of
    's' -> periodInSeconds
    'm' -> periodInSeconds / secondsInAMinute
    'h' -> periodInSeconds / secondsInAnHour
    'd' -> periodInSeconds / secondsInADay
    'w' -> periodInSeconds / secondsInAWeek
    'y' -> periodInSeconds / secondsInAYear
    _   -> periodInSeconds

howManyEarthMasses :: Mass -> Quantity
howManyEarthMasses mass =
  mass / earthMass

howManyAU :: Distance -> Quantity
howManyAU meters =
  meters / au

-- Swarzschild Radius

calculateSchwarzschildRadius :: Mass -> Distance
calculateSchwarzschildRadius mass =
  (2 * gravitationalConstant * mass) / speedOfLight^2

-- Galaxy mass estimator

estimateGalaxyMassVirial :: Velocity -> Distance -> Mass
estimateGalaxyMassVirial stellarVelocityDispersion radiusLightYears =
    (3 * stellarVelocityDispersion^2 * radiusMeters) / gravitationalConstant
    where radiusMeters = lightYearsToMeters radiusLightYears

-- Orbital Calculations

calculateOrbitalPeriod :: Distance -> Mass -> Mass -> Time
calculateOrbitalPeriod semiMajorAxis centralMass orbitingMass =
    sqrt ((4 * pi^2 * semiMajorAxis^3) / (gravitationalConstant * (centralMass + orbitingMass)))

calculateOrbitalVelocity :: Distance -> Time -> Velocity
calculateOrbitalVelocity semiMajorAxis orbitalPeriod =
  (2 * pi * semiMajorAxis) / orbitalPeriod

-- Escape Velocity

calculateEscapeVelocity :: Mass -> Distance -> Velocity
calculateEscapeVelocity objectMass distanceFromObjectCenter =
  sqrt ((2 * gravitationalConstant * objectMass) / distanceFromObjectCenter)

-- Tidal Force

calculateTidalForce :: Mass -> Mass -> Distance -> Distance -> Force
calculateTidalForce largerObjectMass smallerObjectMass
                    differenceBetweenTwoPointsOnSmallerObject
                    distanceBetweenCenterOfBothObjects =
  (2 * gravitationalConstant * largerObjectMass *
  smallerObjectMass * differenceBetweenTwoPointsOnSmallerObject)
  / distanceBetweenCenterOfBothObjects^3

-- Luminosity

calculateLuminosity :: Distance -> Temperature -> Luminosity
calculateLuminosity starRadius starSurfaceTemperature =
  4 * pi * starRadius^2 * stefanBoltzmannConstant * starSurfaceTemperature^4

calculateLuminosityInSolarUnits :: Distance -> Temperature -> Luminosity
calculateLuminosityInSolarUnits starRadius starSurfaceTemperature =
  calculateLuminosity starRadius starSurfaceTemperature / solarLuminosity

-- Hubble Expansion

calculateHubbleExpansion :: Distance -> Velocity
calculateHubbleExpansion properDistance =
  hubblesConstant * properDistance

-- Stellar Mass From Luminosity

calculateStellarMassFromLuminosity :: Luminosity -> Mass
calculateStellarMassFromLuminosity luminosity =
  solarMass * (luminosity / solarLuminosity) ** (1/3.5)

-- Luminosity from Stellar Mass

calculateStellarLuminosityFromMass :: Mass -> Luminosity
calculateStellarLuminosityFromMass mass =
  solarLuminosity * (mass / solarMass) ** 3.5

-- Gravitational Time Dilation

calculateGravitationalTimeDilationFactor :: Mass -> Distance -> Factor
calculateGravitationalTimeDilationFactor mass distanceFromObjectCenter =
  sqrt (1 - (2 * gravitationalConstant * mass) / (distanceFromObjectCenter * speedOfLight^2))

calculateDilatedTime :: Time -> Mass -> Distance -> Time
calculateDilatedTime timeInterval mass distanceFromObjectCenter =
  timeInterval * calculateGravitationalTimeDilationFactor mass distanceFromObjectCenter

-- Lorentz Factor

calculateLorentzFactor :: Velocity -> Factor
calculateLorentzFactor speedOfMovingObserver =
  1 / sqrt(1 - (speedOfMovingObserver^2/speedOfLight^2))

-- Roche Limit

calculateRocheLimit :: Distance -> Density -> Density -> Distance
calculateRocheLimit primaryBodyRadius primaryBodyDensity satelliteDensity =
  primaryBodyRadius * ((2 * primaryBodyDensity) / satelliteDensity) ** (1/3)

-- Photon Sphere Radius

calculatePhotonSphereRadius :: Mass -> Distance
calculatePhotonSphereRadius mass =
  1.5 * calculateSchwarzschildRadius mass

-- Star Lifetime

calculateStarLifetimeInYears :: Mass -> Luminosity -> Time
calculateStarLifetimeInYears starMass starLuminosity =
  (starMass / solarMass) / (starLuminosity / solarLuminosity) * solarLifetimeInYears

calculateStarLifetimeFromLuminosityInYears :: Luminosity -> Time
calculateStarLifetimeFromLuminosityInYears starLuminosity =
  calculateStarLifetimeInYears starMass starLuminosity
  where starMass = calculateStellarMassFromLuminosity starLuminosity

calculateStarLifetimeFromMassInYears :: Mass -> Time
calculateStarLifetimeFromMassInYears starMass =
  calculateStarLifetimeInYears starMass starLuminosity
  where starLuminosity = calculateStellarLuminosityFromMass starMass

-- TODO: Habitable Zone

-- TODO: Peak Wave Length
