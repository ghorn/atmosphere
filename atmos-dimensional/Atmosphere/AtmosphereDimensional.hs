{-# OPTIONS_GHC -Wall #-}

module Atmosphere.AtmosphereDimensional( atmosphere
                                       , atmosphere'
                                       , Atmos(..)
                                       ) where

import Atmosphere hiding (Atmos(..))

import qualified Prelude
import Numeric.Units.Dimensional.Prelude
import Numeric.NumType(Zero, Pos2, Neg1)

type DKinematicViscosity = Dim Pos2 Zero Neg1 Zero Zero Zero Zero
type KinematicViscosity = Quantity DKinematicViscosity

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
                 , density *~ (kilo gram / (meter*meter*meter))
                 , asound *~ (meter / second)
                 , viscosity *~ (newton * second / (meter*meter))
                 , kinematicViscosity *~ (meter*meter/second)
                 )
  where
    (temp, pressure, density, asound, viscosity, kinematicViscosity) = siAtmosphere (alt /~ meter)


atmosphere' :: (Floating a, Ord a) => Length a -> Atmos a
atmosphere' alt = Atmos temp pressure density asound visc kVisc
  where
    (temp, pressure, density, asound, visc, kVisc) = atmosphere alt
