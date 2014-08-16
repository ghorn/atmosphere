{-# OPTIONS_GHC -Wall #-}

module Atmosphere.Dimensional( Atmos(..)
                             , atmosphere
                             ) where

import qualified Atmosphere as A

import qualified Prelude ()
import Numeric.Units.Dimensional.Prelude

data Atmos a = Atmos { atmosTemperature :: ThermodynamicTemperature a
                     , atmosPressure :: Pressure a
                     , atmosDensity :: Density a
                     , atmosSpeedOfSound :: Velocity a
                     , atmosViscosity :: DynamicViscosity a
                     , atmosKinematicViscosity :: KinematicViscosity a
                     }

atmosphere :: (Floating a, Ord a) => Length a -> Atmos a
atmosphere alt = Atmos
                 { atmosTemperature        = temp *~ kelvin
                 , atmosPressure           = pressure *~ pascal
                 , atmosDensity            = density *~ (kilo gram / meter ^ pos3)
                 , atmosSpeedOfSound       = asound *~ (meter / second)
                 , atmosViscosity          = viscosity *~ (newton * second / meter ^ pos2)
                 , atmosKinematicViscosity = kinematicViscosity *~ (meter ^ pos2 / second)
                 }
  where
    A.Atmos temp pressure density asound viscosity kinematicViscosity = A.siAtmosphere (alt /~ meter)
