module Astro.Constants where
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
  hubblesConstant = 2.184e-18 -- s^-1

  wienDisplacementConstant :: Fractional a => a
  wienDisplacementConstant = 2.89777e-3

  innerStellarFlux :: Fractional a => a
  innerStellarFlux = 1.1

  outerStellarFlux :: Fractional a => a
  outerStellarFlux = 0.53