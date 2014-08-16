{-# OPTIONS_GHC -Wall #-}

module Atmosphere.Math
       ( bisection
       ) where

-- We could import this functionality from one of several libraries,
-- but it is very simple so we will save the dependency. Taken from:
-- https://www.fpcomplete.com/user/Sam567/computational-physics/beginner-s-tools/root-finding
bisection :: (Fractional a, Ord a) => a -> (a -> a) -> a -> a -> a
bisection epsilon f a b =
  let av = (b + a)/2
      --the middle of the interval.
      fa = f a
      --the function at a
      fav = f av
      --the function at av
   in  if (b-a < epsilon)
       --this means we are already close enough.
       --In this case, we choose the less biased
       --guess, which is av.
       then av
       else if (fa * fav < 0)
            --this means the root is in the
            --interval (a,av)
            then bisection epsilon f a av
            else if (fav == 0)
            --this is almost impossible if your
            --initial intervals don't have any
            --relationship to the root, but in
            --practice, it happens.
                    then av
                    else bisection epsilon f av b
                    --at this point, we know that
                    --the root is in the interval (av,b)
