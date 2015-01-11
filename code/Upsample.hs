-- Credit to Daniel Winograd-Cort

{-# LANGUAGE Arrows, TupleSections, ExistentialQuantification, ScopedTypeVariables #-}

module Upsample (upsampleI, upsampleH) where

import Euterpea hiding (upsample)
import Control.CCA.ArrowP
import Control.CCA.Types

----------------------------
--------- Upsample ---------
----------------------------

-- Note that both upsample functions essentially apply a unit delay 
-- to the underlying lower sampled arrow.


-- The upsampleH function is an unsampler that holds (hence "H") the 
-- upsampled values for as many time steps as necessary.  Essentially, 
-- it turns the given arrow into a step function at its new clockrate.
upsampleH :: forall a p1 p2 x b. (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1, 
              Clock p2, AudioSample b) => ArrowP a p1 x b -> ArrowP a p2 x b
upsampleH f = g 
   where g = proc x -> do 
               rec cc <- delay 0 -< if cc >= r-1 then 0 else cc+1
                 -- Note that the "delay zero" in the next line prevents upsampling 
                 -- from being instantaneous.  It is unnecessary here, but it is in 
                 -- place to match the behavior of upsampleI
               periodicProbe zero (delay zero <<< ArrowP (strip f)) -< (x,cc == 0)
         r = if outRate < inRate 
             then error "Cannot upsample a signal of higher rate to lower rate" 
             else outRate / inRate
         inRate  = rate (undefined :: p1)
         outRate = rate (undefined :: p2)


-- The upsampleI function is an unsampler that interpolates (hence "I") 
-- the upsampled values.
-- Note that interpolating requires that the output type be Fractional.  
-- Currently, unless you provide (Double, Double) with an instance of Fractional 
-- or add more instances for AudioSample, this restriction means that upsampleI 
-- only works with Mono arrows (i.e. arrows whose output type is Double).
upsampleI :: forall a p1 p2 x b. (ArrowChoice a, ArrowInitP a p1, ArrowInitP a p2, Clock p1, 
              Clock p2, AudioSample b, Fractional b) => ArrowP a p1 x b -> ArrowP a p2 x b
upsampleI f = g 
   where g = proc x -> do 
               rec cc <- delay 0 -< if cc >= r-1 then 0 else cc+1
               now  <- periodicProbe zero (ArrowP (strip f)) -< (x,   cc == 0)
               prev <- periodicProbe zero (delay zero)       -< (now, cc == 0)
               outA -< prev + (now-prev)*(realToFrac $ cc/r)
         r = if outRate < inRate 
             then error "Cannot upsample a signal of higher rate to lower rate" 
             else outRate / inRate
         inRate  = rate (undefined :: p1)
         outRate = rate (undefined :: p2)


-- The periodic probe function takes as arguments an initial value and a 
-- arrow to periodically probe.  When the streaming boolean value is True, 
-- the given arrow is provided with the streaming input, and otherwise it 
-- is left untouched.  The return stream is essentially a step function of 
-- the given arrow.
periodicProbe :: (ArrowChoice a, ArrowInit a) => y -> a x y -> a (x,Bool) y
periodicProbe i a = proc (x,b) -> do
  rec y <- if b then a -< x
                else constA i -< ()
      t1 <- delay i -< if b then y else t1
  outA -< if b then y else t1
