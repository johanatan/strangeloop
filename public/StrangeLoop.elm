-- Copyright (c) 2012 - Jonathan Leonard
--

module StrangeLoop where

import Window as Window
import Time (every)
import Dict
import Maybe
import Config

keys = sampleOn (every ((/) 1 keysRate)) keyInput

controls = lift (\(KeyInput space zoom pan tilt) -> (zoom, pan, tilt)) keys
zoomControls = lift (\(z, _, _) -> z) controls
panControls = lift (\(_, p, _) -> p) controls
tiltControls = lift (\(_, _, t) -> t) controls

stepControl c1 (c0, c, control) =
   let (_, _, adjuster) = fromJust (lookup control keysLookup) in
   case c0 of
   { Positive -> (c1, adjuster (+) c, control)
   ; Negative -> (c1, adjuster (-) c, control)
   ; Neutral  -> (c1, c, control)
   }
zoom = lift snd $ foldp stepControl (Neutral, maxZoom, "Zoom") zoomControls
pan = lift snd $ foldp stepControl (Neutral, 0.0, "Pan") panControls
tilt = lift snd $ foldp stepControl (Neutral, 0.0, "Tilt") tiltControls

cameraPosition = lift3 (\z p t -> (z, p, t)) zoom pan tilt

phi = 1.6180339887
fitWidth height = (floor ((*) (toFloat height) phi))
fitHeight width = (floor ((/) (toFloat width) phi))

area (w,h) = w * h

scaleView (w,h) scale =
   let { nw = (floor ((toFloat w) * scale))
       ; nh = (floor ((toFloat h) * scale))
       ; sizeByWidth = (nw, fitHeight nw)
       ; sizeByHeight = (fitWidth nh, nh) } in
   if area sizeByWidth < area sizeByHeight then sizeByWidth
   else sizeByHeight

generateCorridor w h scale center =
   let (nw,nh) = scaleView (w,h) scale in
   if area (nw,nh) > 1 then
      let { c = if isNothing center then ((nw / 2),(nh / 2)) else fromJust center
          ; (_,_,corridor) = generateCorridor nw nh scale (Just c) } in
      (nw,nh,[ outlined black (rect nw nh c) ] ++ corridor)
   else (nw,nh,[])

scene (w,h) cpos =
   let (nw,nh,corridor) = generateCorridor w h (fst cpos) Nothing in
   container w h middle $ collage nw nh corridor

view = lift2 scene Window.dimensions cameraPosition

--done = lift (\_ -> castBoolToJSBool True) view
--foreign export jsevent "finished"
--  done :: Signal JSBool

main = view
