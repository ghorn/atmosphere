{-# OPTIONS_GHC -Wall #-}

module Atmosphere.Constants
       ( _FT2METERS
       , _KELVIN2RANKINE
       , _PSF2NSM
       , _SCF2KCM
       , _TZERO
       , _PZERO
       , _RHOZERO
       , _AZERO
       , _BETAVISC
       , _SUTH
       , htpgTable
       ) where

import Data.List( zip4 )

_FT2METERS :: (Ord a, Floating a) => a
_KELVIN2RANKINE :: (Ord a, Floating a) => a
_PSF2NSM :: (Ord a, Floating a) => a
_SCF2KCM :: (Ord a, Floating a) => a
_TZERO :: (Ord a, Floating a) => a
_PZERO :: (Ord a, Floating a) => a
_RHOZERO :: (Ord a, Floating a) => a
_AZERO :: (Ord a, Floating a) => a
_BETAVISC :: (Ord a, Floating a) => a
_SUTH :: (Ord a, Floating a) => a

-- | mult. ft. to get meters (exact)
_FT2METERS = 0.3048
-- | mult deg K to get deg R
_KELVIN2RANKINE = 1.8
-- | mult lb/sq.ft to get sq.m
_PSF2NSM = 47.880258
-- | mult slugs/cu.ft to get kg/cu.m
_SCF2KCM = 515.379
-- | sea-level temperature, kelvins
_TZERO   = 288.15
-- | sea-level pressure, N/sq.m
_PZERO   = 101325.0
-- | sea-level density, kg/cu.m
_RHOZERO = 1.225
-- | speed of sound at S.L.  m/sec
_AZERO   = 340.294
-- | viscosity constant
_BETAVISC = 1.458E-6
-- | Sutherland's constant, kelvins
_SUTH    = 110.4

-- | table of (height, temperature, pressure, temperature gradient) over altitude
htpgTable :: (Floating a, Ord a) => [(a,a,a,a)]
htpgTable = zip4 htab ttab ptab gtab
  where
    htab = [ 0.0,  11.0, 20.0, 32.0, 47.0
           , 51.0, 71.0, 84.852 ]
    ttab = [ _TZERO, 216.65, 216.65, 228.65, 270.65
           , 270.65, 214.65, 186.946 ]
    ptab = [ 1.0, 2.2336110E-1, 5.4032950E-2, 8.5666784E-3, 1.0945601E-3
           , 6.6063531E-4, 3.9046834E-5, 3.68501E-6 ]
    gtab = [ -6.5, 0.0, 1.0, 2.8, 0.0, -2.8, -2.0, 0.0 ]
