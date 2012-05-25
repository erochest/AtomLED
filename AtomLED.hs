--
-- Blink a LED on the Arduino Duemilanove using IDE libs from Atom/Haskell.
-- (C) 2010 & 2011 Lee Pike & Tim Dysinger
-- BSD3
--

module Main where

import Language.Atom
import Data.Word
import System.Directory

lowPin :: Word8
lowPin = 2

highPin :: Word8
highPin = 9

high :: Word8
high = 255

low :: Word8
low = 0

digitalWrite :: UE -> UE -> Atom ()
digitalWrite pin level = action dw [pin, level]
    where
        -- Umm. Why do the arguments get switched at this point?
        dw [l, p] = "digitalWrite(" ++ p ++ "," ++ l ++ ")"

vue :: V a -> UE
vue = UVRef . uv

-- | Simple Atom to toggle an LED, leaving it on 8 times as long as it's off.
blink :: Atom ()
blink = do
    pin   <- word8 "pin"   lowPin
    level <- word8 "level" high
    delta <- word8 "delta" 1
    wait  <- word8 "wait"  0

    period ph $ phase 0 $ atom "lp" $ do

        atom "blink" $ do
            cond $ (value wait) ==. 0
            digitalWrite (vue pin) (vue level)
            pin <== (value pin) + (value delta)

            atom "reset_low" $ do
                cond $ (value pin) <. (value lp)
                level <== Const high
                delta <== Const 1
                wait  <== Const waitCount

            atom "reset_high" $ do
                cond $ (value pin) >. (value hp)
                level <== Const low
                delta <== Const (-1)
                wait  <== Const waitCount

        atom "decr_wait" $ do
            cond ((value wait) >. 0)
            decr wait

    where ph        = 10000
          waitCount = 5
          lp        = word8' "lowPin"
          hp        = word8' "highPin"

-- | Invoke the Atom compiler.
main :: IO ()
main = do
    (sch, _, _, _, _) <- compile atomName defaults {cCode = prePostCode} blink
    putStrLn $ reportSchedule sch
    renameFile (atomName ++ ".c") (atomName ++ ".pde")
    where
        atomName = "AtomLED"
        varInit t var val = cType t ++ " " ++ var ++ " = " ++ val ++ ";"
        prePostCode _ _ _ =
            ( unlines [ (varInit Int16 "lowPin" $ show lowPin)
                      , (varInit Int16 "highPin" $ show highPin)
                      ]
            , unlines [ "void setup() {"
                      , "  // initialize the digital pin as an output:"
                      , "  for (int i=lowPin; i<=highPin; i++) {"
                      , "    pinMode(i, OUTPUT);"
                      , "  }"
                      , "}"
                      , ""
                      , "void loop() {"
                      ,  "  " ++ atomName ++ "();"
                      , "}"
                      ]
            )
