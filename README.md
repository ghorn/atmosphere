1976 US Standard Atmosphere
===

[![Build Status](https://secure.travis-ci.org/ghorn/atmosphere.png?branch=master)](http://travis-ci.org/ghorn/atmosphere)

Adapted by Greg Horn from original programs by Ralph L. Carmichael, Public Domain Aeronautical Software
The original code can be found at <http://www.pdas.com/atmos.html>

There are three modules you will be interested in.

    Atmosphere               - port of the atmos code from pdas
    Atmosphere.Dimensional   - same thing with dimensional wrappers
    Atmosphere.DimensionalTF - same thing with dimensional-tf wrappers

The core functionality in all of this is you input altitude and receive
(pressure, density, speed of sound, viscosity, kinematic viscosity).
You can do this in SI or Imperial units with Atmosphere, or you can
use one of the dimensional versions to handle units for you.

As of atmos-0.3.0.0, GHC 7.6 is not supported. If you need it,
use atmos-0.2.0.0 which has no dependencies except for base.
