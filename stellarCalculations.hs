-- Constants

au :: Floating a => a
au = 1.496e11 -- meters

earthMass :: Floating a => a
earthMass = 5.972e24 -- kg

solarMass :: Floating a => a
solarMass = 1.989e30 -- kg

milkyWaySolarMasses :: Floating a => a
milkyWaySolarMasses = 1.5e12 -- kg

gravitationalConstant :: Floating a => a
gravitationalConstant = 6.674e-11

lightYearInMeters :: Floating a => a
lightYearInMeters = 9.461e15 -- m

speedOfLight :: Floating a => a
speedOfLight = 299792458 -- m/s

stefanBoltzmannConstant :: Floating a => a
stefanBoltzmannConstant = 5.67e-8 -- W m^-2 K^-4

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
    'm' -> periodInSeconds / 60
    'h' -> (periodInSeconds / 60) / 60
    'd' -> ((periodInSeconds / 60) / 60) / 24
    'w' -> (((periodInSeconds / 60) / 60) / 24) / 7
    'y' -> (periodInSeconds / 31557600)
    _   -> periodInSeconds

-- Swarzschild Radius

schwarzschildRadius :: (Floating a, Show a) => a -> String
schwarzschildRadius mass = 
  show ((2 * gravitationalConstant * mass) / speedOfLight^2) ++ " meters."

-- How many Earth masses

howManyEarthMasses :: (Floating a, Show a) => a -> String
howManyEarthMasses mass = 
  show (mass / earthMass) ++ " Earth masses."

-- How many AU

howManyAU :: (Floating a, Show a) => a -> String
howManyAU meters = show (meters / au) ++ " AU."

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
