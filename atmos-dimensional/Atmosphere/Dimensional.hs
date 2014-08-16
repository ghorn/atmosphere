{-# OPTIONS_GHC -Wall #-}

module Atmosphere.Dimensional( atmosphere
                             , atmosphere'
                             , Atmos(..)
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

atmosphere :: (Floating a, Ord a) =>
              Length a ->
              (ThermodynamicTemperature a, Pressure a, Density a, Velocity a,
               DynamicViscosity a, KinematicViscosity a)
atmosphere alt = ( temp *~ kelvin
                 , pressure *~ pascal
                 , density *~ (kilo gram / meter ^ pos3)
                 , asound *~ (meter / second)
                 , viscosity *~ (newton * second / meter ^ pos2)
                 , kinematicViscosity *~ (meter ^ pos2 / second)
                 )
  where
    (temp, pressure, density, asound, viscosity, kinematicViscosity) = A.siAtmosphere (alt /~ meter)


atmosphere' :: (Floating a, Ord a) => Length a -> Atmos a
atmosphere' alt = Atmos temp pressure density asound visc kVisc
  where
    (temp, pressure, density, asound, visc, kVisc) = atmosphere alt
