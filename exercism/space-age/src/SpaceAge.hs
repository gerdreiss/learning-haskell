module SpaceAge (Planet(..), ageOn) where

-- Given an age in seconds, calculate how old someone would be on:
-- > Mercury: orbital period 0.2408467 Earth years
-- > Venus:   orbital period 0.61519726 Earth years
-- > Earth:   orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
-- > Mars:    orbital period 1.8808158 Earth years
-- > Jupiter: orbital period 11.862615 Earth years
-- > Saturn:  orbital period 29.447498 Earth years
-- > Uranus:  orbital period 84.016846 Earth years
-- > Neptune: orbital period 164.79132 Earth years

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds =
  case planet of
    Mercury -> earthYears / 0.2408467
    Venus   -> earthYears / 0.61519726
    Earth   -> earthYears / 1.0
    Mars    -> earthYears / 1.8808158
    Jupiter -> earthYears / 11.862615
    Saturn  -> earthYears / 29.447498
    Uranus  -> earthYears / 84.016846
    Neptune -> earthYears / 164.79132
  where
    earthYears = seconds / 31557600

