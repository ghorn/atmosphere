{-# OPTIONS_GHC -Wall #-}

module Atmosphere.Atmosphere( Atmos(..)
                            , siAtmosphere
                            , usAtmosphere
                            , siAtmosphere'
                            , usAtmosphere'
                            , atmosphere
                            , siAltitudeFromPressure
                            ) where

import Atmosphere.Constants
import Atmosphere.Math

data Atmos a = Atmos { atmosTemperature :: a
                     , atmosPressure :: a
                     , atmosDensity :: a
                     , atmosSpeedOfSound :: a
                     , atmosViscosity :: a
                     , atmosKinematicViscosity :: a
                     }

{- |
   atmosphere in SI units

   Input: altitude in meters
   
   Output: (pressure, density, speed of sound, viscosity, kinematic viscosity)

   > pressure            - N/m^2
   > density             - kg/m^3
   > speed of sound      - m/s
   > viscosity           - N-s/m^2
   > kinematic viscosity - m^2/s
-}
siAtmosphere :: (Floating a, Ord a) => a -> (a,a,a,a,a,a)
siAtmosphere alt_m = (temp, pressure, density, asound, viscosity, kinematicViscosity)
  where
    alt_km = 0.001*alt_m
    (sigma, delta, theta) = atmosphere alt_km
    temp = _TZERO * theta
    pressure = _PZERO * delta
    density = _RHOZERO * sigma
    asound = _AZERO * sqrt theta
    viscosity = metricViscosity theta
    kinematicViscosity = viscosity/density

{- |
   atmosphere in imperial units
   
   Input: altitude in ft

   Output: (pressure, density, speed of sound, viscosity, kinematic viscosity)
   
   > pressure            - lb/ft^2
   > density             - slugs/ft^3
   > speed of sound      - ft/s
   > viscosity           - slugs/(ft-s)
   > kinematic viscosity - ft^2/s
-}
usAtmosphere :: (Floating a, Ord a) => a -> (a,a,a,a,a,a)
usAtmosphere alt_ft = (temp, pressure, density, asound, viscosity, kinematicViscosity)
  where
    alt_km = 0.001*_FT2METERS*alt_ft
    (sigma, delta, theta) = atmosphere alt_km
    temp = _KELVIN2RANKINE*_TZERO*theta
    pressure = _PZERO*delta/47.88
    density = _RHOZERO*sigma/515.379
    asound = (_AZERO/_FT2METERS)*sqrt theta
    viscosity=(1.0/_PSF2NSM)*metricViscosity theta
    kinematicViscosity = viscosity/density

metricViscosity :: (Floating a, Ord a) => a -> a
metricViscosity theta = _BETAVISC*sqrt(t*t*t)/(t+_SUTH)
  where
    t = theta * _TZERO

-- | atmosphere in SI units with ADT output
siAtmosphere' :: (Floating a, Ord a) => a -> Atmos a
siAtmosphere' alt = Atmos temp pressure density asound viscosity kinematicViscosity
  where
    (temp, pressure, density, asound, viscosity, kinematicViscosity) = siAtmosphere alt


-- | atmosphere in imperial units with ADT output
usAtmosphere' :: (Floating a, Ord a) => a -> Atmos a
usAtmosphere' alt = Atmos temp pressure density asound viscosity kinematicViscosity
  where
    (temp, pressure, density, asound, viscosity, kinematicViscosity) = usAtmosphere alt


{- |
   Compute temperature, density, and pressure in standard atmosphere.

   Correct to 86 km.  Only approximate thereafter.
   
   Input: alt geometric altitude, km.
   
   Output: (sigma, delta, theta)
   
   > sigma - density/sea-level standard density
   > delta - pressure/sea-level standard pressure
   > theta - temperature/sea-level std. temperature
-}
atmosphere :: (Floating a, Ord a) => a -> (a,a,a)
atmosphere alt = (sigma, delta, theta)
  where
    _REARTH = 6369.0             -- radius of the Earth (km)
    _GMR = 34.163195

    h = alt*_REARTH/(alt+_REARTH) -- geometric to geopotential altitude

    (htabI, tbase, ptabI, tgradI) = getI htpgTable
      where
        getI [htab'] = htab'
        getI (htab0:htab1@(h',_,_,_):htabs)
          | h > h'    = getI (htab1:htabs)
          | otherwise = htab0
        getI [] = error "something went wrong"

    deltah = h - htabI              -- height above local base
    tlocal = tbase + tgradI*deltah  -- local temperature
    
    theta  =  tlocal/_TZERO    -- temperature ratio

    delta
      | 0.0 == tgradI = ptabI*exp(-_GMR*deltah/tbase)
      | otherwise     = ptabI*(tbase/tlocal)**(_GMR/tgradI)
    sigma = delta/theta

{- |
   Compute altitude at which the standard atmosphere has a certain pressure.

   Input: Pressure, N/m^2

   Output: Altitude in meters
-}
siAltitudeFromPressure :: (Floating a, Ord a) => a -> a
siAltitudeFromPressure pressure = bisection 1e-3 ((subtract pressure) . atmosPressure . siAtmosphere') (-1e4) (1e5)
