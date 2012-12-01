-- Copyright (c) 2012 - Jonathan Leonard
-- All rights reserved.

module Config where

import Keyboard.Raw
import JavaScript
import Dict
import Maybe

frameRate = 40 -- in Hz. display frame rate.
keysRate = 250 -- in Hz. keyboard sample rate.

zoomRate = 1.1 -- in x per sec (as in 2x, 3x, etc).
zoomDelta = (/) zoomRate keysRate -- in x per key sample.
minZoom = 0.125
maxZoom = 0.9999

ptRate = pi / 8.0   -- in radians per sec (22.5 degrees).
ptDelta = (/) ptRate keysRate -- in radians per key sample.
maxPt = pi / 4.0    -- positive 45 degrees.
minPt = (-) 0 maxPt -- negative 45 degrees.

fps = constant $ castIntToJSNumber frameRate

foreign export jsevent "fps"
   fps :: Signal JSNumber

foreign import jsevent "trigger" (castIntToJSNumber 0)
   jsTime :: Signal JSNumber

time = lift castJSNumberToFloat jsTime
delta = lift snd $ foldp (\t1 (t0, d) -> (t1, t1 - t0)) (0, 0) time

foreign import jsevent "webcamFrame" (castStringToJSString "")
   jsCanvasUrl :: Signal JSString

frames = lift castJSStringToString jsCanvasUrl

data Control = Positive | Neutral | Negative
data KeyInput = KeyInput Bool Control Control Control
defaultKeyInput = KeyInput False Neutral Neutral Neutral

adjust op value min max delta = (clamp min max (op value delta))
adjustZoom op zoom = adjust op zoom minZoom maxZoom zoomDelta
adjustPt op pt = adjust op pt minPt maxPt ptDelta

keysLookup = fromList [ ("Zoom", (81, 65, adjustZoom)),
                        ("Pan", (39, 37, adjustPt)),
                        ("Tilt", (38, 40, adjustPt)) ]

fromJust m = fromMaybe undefined m

updateInput control key value =
   let (positiveKey, negativeKey, _) =  fromJust (lookup control keysLookup) in
   case value of
   { Positive -> if key == negativeKey then Neutral else Positive
   ; Negative -> if key == positiveKey then Neutral else Negative
   ; Neutral  -> if key == positiveKey then Positive else
                 if key == negativeKey then Negative else Neutral
   }
updateZoom key value = updateInput "Zoom" key value
updatePan key value = updateInput "Pan" key value
updateTilt key value = updateInput "Tilt" key value

updateInputs key (KeyInput pause zoom pan tilt) =
   KeyInput (pause || key == 32) (updateZoom key zoom) (updatePan key pan) (updateTilt key tilt)
keyInput = dropRepeats $ lift (List.foldl updateInputs defaultKeyInput) keysDown
