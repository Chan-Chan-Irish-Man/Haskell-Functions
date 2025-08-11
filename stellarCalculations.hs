-- Constants
secondsInAMinute :: Floating a => a
secondsInAMinute = 60

secondsInAnHour :: Floating a => a
secondsInAnHour = 3600

secondsInADay :: Floating a => a
secondsInADay = 86400

secondsInAWeek :: Floating a => a
secondsInAWeek = 60480

secondsInAYear :: Floating a => a
secondsInAYear = 31557600

au :: Floating a => a
au = 1.496e11 -- meters

earthMass :: Floating a => a
earthMass = 5.972e24 -- kg

solarMass :: Floating a => a
solarMass = 1.989e30 -- kg

solarLuminosity :: Floating a => a
solarLuminosity = 1.0

milkyWaySolarMasses :: Floating a => a
milkyWaySolarMasses = 1.5e12 -- solar masses

gravitationalConstant :: Floating a => a
gravitationalConstant = 6.674e-11

lightYearInMeters :: Floating a => a
lightYearInMeters = 9.461e15 -- m

speedOfLight :: Floating a => a
speedOfLight = 299792458 -- m/s

stefanBoltzmannConstant :: Floating a => a
stefanBoltzmannConstant = 5.67e-8 -- W m^-2 K^-4

hubblesConstant :: Floating a => a
hubblesConstant = 674e2 -- m/s/Mpc (as observed by the Planck satellite)

-- Utility functions

lightYearsToMeters :: Floating a => a -> a
lightYearsToMeters lightYears = lightYears * lightYearInMeters

kgToSolarMass :: Floating a => a -> a
kgToSolarMass kg = kg / solarMass

solarMassesToMilkyWayMasses :: Floating a => a -> a
solarMassesToMilkyWayMasses solarMasses = solarMasses / milkyWaySolarMasses

convertPeriod :: Floating a => Char -> a -> a
convertPeriod unit periodInSeconds =
  case unit of
    's' -> periodInSeconds
    'm' -> periodInSeconds / secondsInAMinute
    'h' -> periodInSeconds / secondsInAnHour
    'd' -> periodInSeconds / secondsInADay
    'w' -> periodInSeconds / secondsInAWeek
    'y' -> periodInSeconds / secondsInAYear
    _   -> periodInSeconds

-- Swarzschild Radius

schwarzschildRadius :: Floating a => a -> a
schwarzschildRadius mass = (2 * gravitationalConstant * mass) / speedOfLight^2

-- How many Earth masses
howManyEarthMasses :: Floating a => a -> a
howManyEarthMasses mass = mass / earthMass

-- How many AU
howManyAU :: Floating a => a -> a
howManyAU meters = meters / au

-- Galaxy mass estimator

estimateGalaxyMassVirial :: (Floating a) => a -> a -> a
estimateGalaxyMassVirial stellarVelocityDispersion radiusLightYears =
    (3 * stellarVelocityDispersion^2 * radiusMeters) / gravitationalConstant
    where radiusMeters = lightYearsToMeters radiusLightYears

-- Orbital Calculations

calculateOrbitalPeriod :: Floating a => a -> a -> a -> a
calculateOrbitalPeriod semiMajorAxis centralMass orbitingMass =
    sqrt ((4 * pi^2 * semiMajorAxis^3) / (gravitationalConstant * (centralMass + orbitingMass)))

calculateOrbitalVelocity :: Floating a => a -> a -> a
calculateOrbitalVelocity semiMajorAxis orbitalPeriod =
  (2 * pi * semiMajorAxis) / orbitalPeriod

-- Escape Velocity

calculateEscapeVelocity :: Floating a => a -> a -> a
calculateEscapeVelocity objectMass distanceFromObjectCenter =
  sqrt ((2 * gravitationalConstant * objectMass) / distanceFromObjectCenter)

-- Tidal Force

calculateTidalForce :: Floating a => a -> a -> a -> a -> a
calculateTidalForce largerObjectMass smallerObjectMass
                    differenceBetweenTwoPointsOnSmallerObject
                    distanceBetweenCenterOfBothObjects =
  (2 * gravitationalConstant * largerObjectMass *
  smallerObjectMass * differenceBetweenTwoPointsOnSmallerObject)
  / distanceBetweenCenterOfBothObjects^3

-- Luminosity

calculateLuminosity :: Floating a => a -> a -> a
calculateLuminosity starRadius starSurfaceTemperature =
  (4 * pi * starRadius^2 * stefanBoltzmannConstant * starSurfaceTemperature^4)

-- Hubble Expansion

calculateHubbleExpansion :: Floating a => a -> a
calculateHubbleExpansion properDistance = hubblesConstant * properDistance

-- Stellar Mass From Luminosity

calculateStellarMassFromLuminosity :: Floating a => a -> a
calculateStellarMassFromLuminosity luminosity = solarMass * (luminosity / solarLuminosity) ** (1/3.5)

-- Gravitational Time Dilation

calculateGravitationalTimeDilationFactor :: Floating a => a -> a -> a
calculateGravitationalTimeDilationFactor mass distanceFromObjectCenter =
  sqrt (1 - (2 * gravitationalConstant * mass) / (distanceFromObjectCenter * speedOfLight^2))

calculateDilatedTime :: Floating a => a -> a -> a -> a
calculateDilatedTime timeInterval mass distanceFromObjectCenter =
  timeInterval * calculateGravitationalTimeDilationFactor mass distanceFromObjectCenter

-- Lorentz Factor

calculateLorentzFactor :: Floating a => a -> a
calculateLorentzFactor speedOfMovingObserver =
  1 / sqrt(1 - (speedOfMovingObserver^2/speedOfLight^2))
