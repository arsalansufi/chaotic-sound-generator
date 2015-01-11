Arsalan Sufi and Kevin Jiang
Chaotic Sound Generator

> {-# LANGUAGE Arrows #-}
> module ChaoticSoundGenerator where
> import Euterpea
> import Euterpea.IO.Audio.Basics
> import Upsample

list of outFile commands ------------------------------------------------------

lorentzSoundOutput
lorentzBluesOutput
lorentsBubbles1Output
lorentzBubbles2Output
rosslerSoundOutput
chuaSoundOutput

Lorentz attractor -------------------------------------------------------------

This function represents the three differential equations of the Lorentz
attractor. Note that the differential equations have been rewritten as integral
equations to make their outputs more accessible. s, r, and b are the equations'
three parameters, and i is the initial value of the x-, y-, and z-outputs.

> lorentz :: Double -> Double -> Double -> Double -> SigFun LorentzCR () (Double, Double, Double)
> lorentz s r b i =
>   proc () -> do
>     rec x <- integral -< s * (y - x) + i
>         y <- integral -< (-1) * y - x * z + r * x + i
>         z <- integral -< x * y - b * z + i
>     outA -< (x, y, z)

the clock rate for the lorentz function

> data LorentzCR
> instance Clock LorentzCR where
>   rate _ = 100

the three outputs of the lorentz function (x, y, and z) each isolated

> lorentzX :: SigFun LorentzCR () Double
> lorentzX =
>   proc () -> do
>     (x, y, z) <- lorentz 10 28 2.667 0.6 -< ()
>     outA -< x

> lorentzY :: SigFun LorentzCR () Double
> lorentzY =
>   proc () -> do
>     (x, y, z) <- lorentz 10 28 2.667 0.6 -< ()
>     outA -< y

> lorentzZ :: SigFun LorentzCR () Double
> lorentzZ =
>   proc () -> do
>     (x, y, z) <- lorentz 10 28 2.667 0.6 -< ()
>     outA -< z

This chaotic sound uses the outputs of the lorentz function to modulate a
stereo sine wave. The y-output is used to modulate frequency. The z-output is
used to modulate amplitude, and the x-output is used to modulate the balance
between the left and right channels.

> lorentzSound :: SigFun AudRate () (Double, Double)
> lorentzSound =
>   let sineTable = tableSinesN 4096 [1]
>   in proc () -> do
>        x <- upsampleI lorentzX -< ()
>        y <- upsampleI lorentzY -< ()
>        z <- upsampleI lorentzZ -< ()
>        sine <- osc sineTable 0 -< (y + 50) * 5
>        outA -< ((((x / 60) - 1) ** 2 / 4) * sine * z / 50, (((x / 60) + 1) ** 2 / 4) * sine * z / 50)

> lorentzSoundOutput = outFile "lorentzSound.wav" 30 lorentzSound

This alternative approach to producing a chaotic sound uses the y-output
differently. Rather than scaling the y-output to bring it into the range of
audible frequencies, the mod of the y-output is taken to randomly select a
pitch from the blues scale rooted at C4.

> lorentzBlues :: SigFun AudRate () (Double, Double)
> lorentzBlues =
>   let sineTable = tableSinesN 4096 [1]
>   in proc () -> do
>        x <- upsampleI lorentzX -< ()
>        y <- upsampleI lorentzY -< ()
>        z <- upsampleI lorentzZ -< ()
>        sine <- osc sineTable 0 -< limitBlues y
>        outA -< ((((x / 60) - 1) ** 2 / 4) * sine * z / 50, (((x / 60) + 1) ** 2 / 4) * sine * z / 50)

> limitBlues f =
>   if      (mod (ceiling f) 7) == 0 then 261.63
>   else if (mod (ceiling f) 7) == 1 then 311.13
>   else if (mod (ceiling f) 7) == 2 then 349.23
>   else if (mod (ceiling f) 7) == 3 then 369.99
>   else if (mod (ceiling f) 7) == 4 then 392.00
>   else if (mod (ceiling f) 7) == 5 then 466.16
>   else if (mod (ceiling f) 7) == 6 then 523.25
>   else 0

> lorentzBluesOutput = outFile "lorentzBlues.wav" 10 lorentzBlues

Below is a variation to the approach described above. Instead of limiting the
pitches to the blues scale, the pitches are limited to C4, E4, F4, and G4. In
addition, the pitch bursts are sent through the echo bounce function that we
wrote for HW9. (The x-output is no longer used to modulate the left-right
balance.) The result sounds somewhat like a bubbling pot.

> lorentzBubbles1 :: SigFun AudRate () (Double, Double)
> lorentzBubbles1 =
>   proc () -> do
>     input <- lorentzBursts1 -< ()
>     (left, right) <- echoBounce 0.25 0.9 -< input
>     outA -< (left, right)

> lorentzBursts1 :: SigFun AudRate () Double
> lorentzBursts1 =
>   let sineTable = tableSinesN 4096 [1]
>   in proc () -> do
>        y <- upsampleI lorentzY -< ()
>        z <- upsampleI lorentzZ -< ()
>        output <- osc sineTable 0 -< limitCEFG (y + 50)
>        outA -< output * z / 50

> limitCEFG f =
>   if      (mod (ceiling f) 4) == 0 then 261.63
>   else if (mod (ceiling f) 4) == 1 then 329.63
>   else if (mod (ceiling f) 4) == 2 then 349.23
>   else if (mod (ceiling f) 4) == 3 then 392.00
>   else 0

> echoBounce :: Double -> Double -> SigFun AudRate Double (Double, Double)
> echoBounce del dec =
>   proc input -> do
>     rec left <- delayLine del -< input + dec * right
>         right <- delayLine del -< dec * left
>     outA -< (left/4, right/4)

> lorentzBubbles1Output = outFile "lorentzBubbles1.wav" 10 lorentzBubbles1

Here's one final approach. It's identical to the approach above except that the
pitch bursts happen less frequently. In addition, the roles of the y- and
z-outputs are swapped. The y-output is used to modulate amplitude, and the
z-output is used to modulate frequency. The result is actually pretty pleasing!

> lorentzBubbles2 :: SigFun AudRate () (Double, Double)
> lorentzBubbles2 =
>   proc () -> do
>     input <- lorentzBursts2 -< ()
>     (left, right) <- echoBounce 0.25 0.9 -< input
>     outA -< (left, right)

> lorentzBursts2 :: SigFun AudRate () Double
> lorentzBursts2 =
>   let sineTable = tableSinesN 4096 [1]
>   in proc () -> do
>        y <- upsampleI lorentzY -< ()
>        z <- upsampleI lorentzZ -< ()
>        output <- osc sineTable 0 -< limitCEFG ((z + 50) / 20)
>        outA -< output * y / 50

> lorentzBubbles2Output = outFile "lorentzBubbles2.wav" 10 lorentzBubbles2

Rossler attractor -------------------------------------------------------------

This function represents the three differential equations of the Rossler
attractor. Note that the differential equations have been rewritten as integral
equations to make their outputs more accessible. u, v, and w are the equations'
three parameters.

> rossler :: Double -> Double -> Double -> SigFun RosslerCR () (Double, Double, Double)
> rossler u v w =
>   proc () -> do
>     rec x <- integral -< (-1) * y - z
>         y <- integral -< x + u * y
>         z <- integral -< v + x * z - w * z
>     outA -< (x, y, z)

the clock rate for the rossler function

> data RosslerCR
> instance Clock RosslerCR where
>   rate _ = 100

the three outputs of the rossler function (x, y, and z) each isolated

> rosslerX :: SigFun RosslerCR () Double
> rosslerX =
>   proc () -> do
>     (x, y, z) <- rossler 0.375 4 4 -< ()
>     outA -< x

> rosslerY :: SigFun RosslerCR () Double
> rosslerY =
>   proc () -> do
>     (x, y, z) <- rossler 0.375 4 4 -< ()
>     outA -< y

> rosslerZ :: SigFun RosslerCR () Double
> rosslerZ =
>   proc () -> do
>     (x, y, z) <- rossler 0.375 4 4 -< ()
>     outA -< z

This chaotic sound uses the outputs of the rossler function to modulate a
stereo sine wave. The z-output is used to modulate frequency. The x-output is
used to modulate amplitude, and the y-output is used to modulate the balance
between the left and right channels.

> rosslerSound :: SigFun AudRate () (Double, Double)
> rosslerSound =
>   let sineTable = tableSinesN 4096 [1]
>   in proc () -> do
>        x <- upsampleI rosslerX -< ()
>        y <- upsampleI rosslerY -< ()
>        z <- upsampleI rosslerZ -< ()
>        sine <- osc sineTable 0 -< (z + 3) * 85
>        outA -< ((((y / 3 + 0.5) - 1) ** 2 / 4) * sine * x / 3, (((y / 3 + 0.5) + 1) ** 2 / 4) * sine * x / 3)

> rosslerSoundOutput = outFile "rosslerSound.wav" 30 rosslerSound

Chua attractor ----------------------------------------------------------------

This function represents the three differential equations of the Chua
attractor. Note that the differential equations have been rewritten as integral
equations to make their outputs more accessible. c1, c2, c3, m0, and m1 are the
equations' five parameters, and i is the initial value of the x-output.

> chua :: Double -> Double -> Double -> Double -> Double -> Double -> SigFun ChuaCR () (Double, Double, Double)
> chua c1 c2 c3 m0 m1 i =
>   proc () -> do
>     rec x <- integral -< c1 * (y - x - (m1 * x + ((m0 - m1) / 2) * (abs(x + 1) - (abs(x - 1))))) + i
>         y <- integral -< c2 * (x - y + z)
>         z <- integral -< (-1) * c3 * y
>     outA -< (x, y, z)

the clock rate for the chua function

> data ChuaCR
> instance Clock ChuaCR where
>   rate _ = 100

the three outputs of the chua function (x, y, and z) each isolated

> chuaX :: SigFun ChuaCR () Double
> chuaX =
>   proc () -> do
>     (x, y, z) <- chua 15.6 1 25.58 (-1.143) 0.714 0.7 -< ()
>     outA -< x

> chuaY :: SigFun ChuaCR () Double
> chuaY =
>   proc () -> do
>     (x, y, z) <- chua 15.6 1 25.58 (-1.143) 0.714 0.7 -< ()
>     outA -< y

> chuaZ :: SigFun ChuaCR () Double
> chuaZ =
>   proc () -> do
>     (x, y, z) <- chua 15.6 1 25.58 (-1.143) 0.714 0.7 -< ()
>     outA -< z

This chaotic sound uses the outputs of the chua function to modulate a stereo
sine wave. The z-output is used to modulate frequency. The y-output is used to
modulate amplitude, and the x-output is used to modulate the balance between
the left and right channels.

> chuaSound :: SigFun AudRate () (Double, Double)
> chuaSound =
>   let sineTable = tableSinesN 4096 [1]
>   in proc () -> do
>        x <- upsampleI chuaX -< ()
>        y <- upsampleI chuaY -< ()
>        z <- upsampleI chuaZ -< ()
>        sine <- osc sineTable 0 -< (z + 2) * 250
>        outA -< ((((x - 1) - 1) ** 2 / 4) * sine * y * 10, (((x - 1) + 1) ** 2 / 4) * sine * y * 10)

> chuaSoundOutput = outFile "chuaSound.wav" 30 chuaSound
