module Astro.Utilities where
  import Control.Monad (guard)
  import Astro.Constants
  import Astro.Synonyms

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

  lightYearsToParsec :: Distance -> Distance
  lightYearsToParsec lightYears =
    lightYears * 3.26156