{-# OPTIONS_GHC -Wall #-}

module Atmosphere.Tests( simpleAtmosVsAtmos
                       ) where

import Atmosphere.Atmosphere

simpleAtmosVsAtmos :: Double
simpleAtmosVsAtmos = maximum $ [maxErr alt | alt <- [0,0.01..19.99]]

maxErr :: (Floating a, Ord a) => a -> a
maxErr x = maximum  (map abs [e0,e1,e2])
  where
    (e0,e1,e2) = err x

err :: (Floating a, Ord a) => a -> (a,a,a)
err x = (s1-s0, d1-d0, t1-t0)
  where
    (s0,d0,t0) = atmosphere x
    (s1,d1,t1) = simpleAtmosphere x
        
simpleAtmosphere :: (Floating a, Ord a) => a -> (a,a,a)
simpleAtmosphere alt = (sigma, delta, theta)
{-
Compute temperature, density, and pressure in simplified
standard atmosphere.

Correct to 20 km.  Only approximate thereafter.

Input:
    alt	geometric altitude, km.
Return: (sigma, delta, theta)
    sigma	density/sea-level standard density
    delta	pressure/sea-level standard pressure
    theta	temperature/sea-level std. temperature
-}
  where
    _REARTH = 6369.0             -- radius of the Earth (km)
    _GMR = 34.163195             -- gas constant

    h = alt*_REARTH/(alt+_REARTH) -- geometric to geopotential altitude

    (theta, delta)
      -- troposphere
      | h < 11.0 = ( (288.15 - 6.5*h)/288.15, theta**(_GMR/6.5) )
      -- stratosphere
      | h < 20.0 = (216.65/288.15, 0.2233611*exp(-_GMR*(h-11.0)/216.65))
      | otherwise = error "simpleAtmosphere invalid higher than 20 km"
    sigma = delta/theta
