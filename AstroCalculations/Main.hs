module Main(main) where
  import Control.Monad (guard)
  import Astro.Constants
  import Astro.Synonyms
  import Astro.BlackHoles
  import Astro.Energy
  import Astro.Galaxies
  import Astro.Orbitals
  import Astro.Stellar
  import Astro.Utilities

  main :: IO ()
  main = do
    putStrLn "Welcome to AstroCalculations!"