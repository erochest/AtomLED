--
-- Blink a LED on the Arduino Duemilanove using IDE libs from Atom/Haskell.
-- (C) 2010 & 2011 Lee Pike & Tim Dysinger
-- BSD3
--

module Main where

import Language.Atom
import Data.Word
import System.Directory

ledPin :: Word8
ledPin = 8

high :: Word8
high = 255

low :: Word8
low = 0

digitalWrite :: Word8 -> Atom ()
digitalWrite = action dw . (:[]) . ue . Const
    where
        dw [l] = "digitalWrite(" ++ show ledPin ++ "," ++ l ++ ")"

-- | Simple Atom to toggle an LED, leaving it on 8 times as long as it's off.
blink :: Atom ()
blink = do
  on <- bool "on" True
  period ph $ phase 0 $ atom "blinkOn" $ do
    digitalWrite 255
    on <== not_ (value on)
  period ph $ phase (quot ph 8) $ atom "blinkOff" $ do
    digitalWrite 0
    on <== not_ (value on)
  where
    ph = 50000

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
      ( unlines
        [ (varInit Int16 "ledPin" $ show ledPin)
        , "void avr_blink(void);"
        ]
      , unlines
        [ "void setup() {"
        , "  // initialize the digital pin as an output:"
        , "  pinMode(ledPin, OUTPUT);"
        , "}"
        , ""
        , "// set the LED on or off"
        , "void avr_blink() { digitalWrite(ledPin, state.AtomLED.on); }"
        , ""
        , "void loop() {"
        ,  "  " ++ atomName ++ "();"
        , "}"
        ]
      )
