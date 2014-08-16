1976 US Standard Atmosphere
===

[![Build Status](https://secure.travis-ci.org/ghorn/atmosphere.png?branch=master)](http://travis-ci.org/ghorn/atmosphere)

Adapted by Greg Horn from original programs by Ralph L. Carmichael, Public Domain Aeronautical Software
The original code can be found at <http://www.pdas.com/atmos.html>

There are three .cabal projects in this repository:
    atmos                - port of the atmos code from pdas 
    atmos-dimensional    - thin wrapper around atmos using dimensional for units
    atmos-dimensional-tf - thin wrapper around atmos using dimensional-tf for units

The core functionality in all of this is you input altitude and recieve
(pressure, density, speed of sound, viscosity, kinematic viscosity).
You can do this in SI or Imperial units with the atmos package, use you can
use one of the dimensional packages to handle units.
