{-# LANGUAGE Arrows #-}
module PhysicalModelling where

import Euterpea

-- Wavetable
  
sineTable :: Table
sineTable = tableSinesN 4096 [1] -- [One period]


--Constructing the InstrMap:

sawName, sqName, triName :: InstrumentName
sawName = CustomInstrument "Sawtooth wave"
sqName = CustomInstrument "Square Wave"
triName = CustomInstrument "Triangle Wave"

myInstrMap :: InstrMap (AudSF () Double)
myInstrMap = [(sawName, sawWave), (sqName,sqWave), (triName,triWave)]

-- Melody construction samples

mel1 = toMusic1 $ line $ map ($en) [c 4, c 5, b 4, a 4, g 4, f 4, e 4, d 4, c 4]

{-
mel2 = note qn (C,4) :+: note qn (E,4) :+: 
     note qn (G,4) :+: note qn (C,5)

mel3 = c 4 qn :+: e 4 qn :+: g 4 qn :+: c 5 qn
-}

sonataInC :: Music Pitch
sonataInC = line [c 5 wn, e 5 hn, g 5 hn, b 4 dhn, c 5 en, d 5 en, c 5 hn, rest hn,
             a 5 wn, g 5 hn, c 6 hn, g 5 hn, f 5 en, g 5 en, e 5 en, f 5 en, e 5 hn, rest hn, 
             a 4 qn, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en, a 5 en,
             g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en,
             g 4 qn, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en,
             f 4 qn, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en,
             e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en,
             e 4 qn, f 4 en, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, 
             d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en, e 4 en,
             d 4 qn, e 4 en, f 4 en, g 4 en, a 4 en, b 4 en, cs 5 en,
             d 5 en, a 4 en, b 4 en, cs 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             a 5 en, b 5 en, c 6 en, b 5 en, a 5 en, g 5 en, f 5 en, e 5 en,
             f 5 en, g 5 en, a 5 en, g 5 en, f 5 en, e 5 en, d 5 en, c 5 en,
             b 4 qn, g 5 qn, e 5 qn, c 5 qn, d 5 qn, g 5 qn, e 5 qn, c 5 qn,
             d 5 hn, g 5 hn, g 4 hn, rest hn,
             fs 4 en, g 4 en, fs 4 en, g 4 en, fs 4 en, g 4 en, fs 4 en, g 4 en,
             f 4 en, g 4 en, f 4 en, g 4 en, f 4 en, g 4 en, f 4 en, g 4 en,
             g 5 qn, e 5 qn, c 5 dhn, d 5 en, e 5 en, d 5 qn, c 5 qn,
             c 5 dqn, b 4 en, b 4 hn, rest wn, g 5 qn, e 5 qn, c 5 dhn,
             d 5 en, e 5 en, d 5 qn, c 5 qn, c 5 dqn, b 4 en, b 4 hn, rest wn,
             g 5 en, e 3 en,g 3 en, c 4 en, e 4 en, g 5 en, e 5 en, c 5 en,
             a 4 en, f 3 en, a 3 en, c 4 en, f 4 en, a 4 en, c 5 en, a 4 en,
             f 5 en, d 3 en, f 3 en, b 3 en, d 4 en, f 5 en, d 5 en, b 4 en,
             g 4 en, e 3 en, g 3 en, b 3 en, e 4 en, g 4 en, b 4 en, g 4 en,
             e 5 en, c 4 en, e 4 en, a 4 en, c 5 en, e 5 en, c 5 en, a 4 en,
             f 4 en, d 4 en, f 4 en, a 4 en, d 5 en, f 4 en, a 4 en, f 4 en,
             d 6 en, b 3 en, d 4 en, g 4 en, b 4 en, d 6 en, b 5 en, g 5 en,
             e 5 en, c 4 en, e 4 en, g 4 en, c 5 en, c 6 en, g 5 en, e 5 en,
             d 5 wn, d 5 hn, d 5 hn, a 5 wn, a 5 hn, a 5 hn, g 5 qn, a 5 en,
             b 5 en, c 6 en, d 6 en, e 6 en, d 6 en, c 6 en, b 5 en, a 5 en,
             g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, e 5 en, d 5 en, e 5 en,
             d 5 en, e 5 en, d 5 en, e 5 en, d 5 en, e 5 en, d 5 en, e 5 en,
             d 5 en, e 5 en, d 5 en, c 5 en, d 5 en, c 5 hn, c 5 en, g 4 en,
             c 5 en, e 5 en, g 5 en, e 5 en, c 5 en, e 5 en, f 5 en, d 5 en,
             b 4 en, d 5 en, c 5 hn, c 4 en, g 3 en, c 4 en, e 4 en, g 4 en,
             e 4 en, c 4 en, e 4 en, f 4 en, d 4 en, b 3 en, d 4 en, c 4 hn,
             c 5 hn, c 4 hn]

--Fourier (Additive) Synthesis--
             
-- Triangle Wave
triWave :: Instr (Mono AudRate)
triWave dur pch vol params =
    let dur' = fromRational dur
        freq = apToHz pch
    in proc _ -> do
       osc1 <- osc sineTable 0 -< freq
       osc2 <- osc sineTable 0 -< freq*3
       osc3 <- osc sineTable 0 -< freq*5
       env1 <- envLineSeg [0, 1.0, 0, 0] [0.05,1,100] -< ()
       env2 <- envLineSeg [0, 0.11, 0, 0] [0.05,2,100] -< () 
       env3 <- envLineSeg [0, 0.0004, 0, 0] [0.05,2,100] -< ()
       let partials = ((osc1*env1) + (osc2*env2) + (osc3*env3)) / 3
       rec square <- delayLine (1/freq)   -< effect
           x   <- delayLine (1/freq/2) -< partials + square*0.4
           effect <- filterLowPassBW -< (x-x*x*x + square*0.4, 2000)
       outA -< 0.95 * partials * effect

triNotes = writeWav "TriangleWaves.wav" myInstrMap
    (tempo 0.4 $ instrument triName mel1)

-- SquareWave
sqWave :: Instr (Mono AudRate)
sqWave dur pch vol params =
    let dur' = fromRational dur
        freq = apToHz pch
    in proc _ -> do
       osc1 <- osc sineTable 0 -< freq
       osc2 <- osc sineTable 0 -< freq*3
       osc3 <- osc sineTable 0 -< freq*5
       osc4 <- osc sineTable 0 -< freq*7
       osc5 <- osc sineTable 0 -< freq*9
       env1 <- envLineSeg [0, 1.0, 0, 0] [0.05,1,100] -< ()
       env2 <- envLineSeg [0, 0.33, 0, 0] [0.05,2,100] -< () 
       env3 <- envLineSeg [0, 0.2, 0, 0] [0.05,2,100] -< ()
       env4 <- envLineSeg [0, 0.143, 0, 0] [0.05,2,100] -< ()
       env5 <- envLineSeg [0, 0.11, 0, 0] [0.05,2,100] -< ()
       let partials = ((osc1*env1) + (osc2*env2) + (osc3*env3) + (osc4*env4) + (osc5*env5)) / 5
       rec square <- delayLine (1/freq)   -< effect
           x   <- delayLine (1/freq/2) -< partials + square*0.4
           effect <- filterLowPassBW -< (x-x*x*x + square*0.4, 2000)
       outA -< 0.95 * partials * effect
      
-- SquareWave Test      
sqNotes = writeWav "SquareWaves.wav" myInstrMap
    (tempo 0.4 $ instrument sqName mel1)
    
-- SawWave Instrument
 
sawWave :: Instr (Mono AudRate)
sawWave dur pch vol params =
    let dur' = fromRational dur
        freq = apToHz pch
    in proc _ -> do
       osc1 <- osc sineTable 0 -< freq
       osc2 <- osc sineTable 0 -< freq*2
       osc3 <- osc sineTable 0 -< freq*3
       osc4 <- osc sineTable 0 -< freq*4
       osc5 <- osc sineTable 0 -< freq*5
       env1 <- envLineSeg [0, 1.0, 0, 0] [0.05,1,100] -< ()
       env2 <- envLineSeg [0, 0.5, 0, 0] [0.05,2,100] -< () 
       env3 <- envLineSeg [0, 0.33, 0, 0] [0.05,2,100] -< ()
       env4 <- envLineSeg [0, 0.25, 0, 0] [0.05,2,100] -< ()
       env5 <- envLineSeg [0, 0.2, 0, 0] [0.05,2,100] -< ()
       let partials = ((osc1*env1) + (osc2*env2) + (osc3*env3) + (osc4*env4) + (osc5*env5)) / 5
       rec saw <- delayLine (1/freq)   -< effect
           x   <- delayLine (1/freq/2) -< partials + saw*0.4
           effect <- filterLowPassBW -< (x-x*x*x + saw*0.4, 2000)
       outA -< 0.95 * partials * effect
       
-- SawWave Test

sawNote = writeWav "sawNoteTransposed.wav" myInstrMap
    (tempo 0.5 $ transpose 12 $ instrument sawName (g 5 wn))
    
sawNotes = writeWav "sawNotesModded.wav" myInstrMap
    (tempo 0.4 $ instrument sawName mel1)
        
sawNata = instrument sawName $ sonataInC

testSaw = writeWav "sawNata.wav" myInstrMap sawNata


-- Physical Modelling Synthesis --
    
-- Karplus-Strong String Synthesis
bnoise :: AudSF () Double
bnoise = proc () -> do
    env1 <- envLineSeg [1,1,0,0] [0.01, 0.000001, 9.8] -< ()
    noise <- noiseWhite 44 -< ()
    outA -< env1 * noise

stringPluck :: AudSF Double Double
stringPluck = proc input -> do
        rec delay <- delayLine 0.01 -< (flt+input)
            flt <- filterLowPass -< (delay, 2000)
        outA -< flt + input
        
stringNote = outFile "ksPluck.wav" 8 $ bnoise >>> stringPluck 
