-- Copyright (c) 2012 - Jonathan Leonard
-- All rights reserved.

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
fit op dim = (floor (op (toFloat dim) phi))
fitWidth height = fit (*) height
fitHeight width = fit (/) width

area (w,h) = w * h

scaleView (w,h) scale =
   let { nw = (floor ((toFloat w) * scale))
       ; nh = (floor ((toFloat h) * scale))
       ; sizeByWidth = (nw, fitHeight nw)
       ; sizeByHeight = (fitWidth nh, nh) } in
   if area sizeByWidth < area sizeByHeight then sizeByWidth
   else sizeByHeight

invertHoriz (x,y) = (0 - x, y)
invertPoly poly = List.map (\e -> (invertHoriz e)) (reverse poly)

generateTrapezoid w h center pan =
   let { halfH = (h / 2)
       ; halfW = (w / 2)
       ; long = ((0 - halfW, 0 - halfH), ((0 - halfW), halfH))
       ; halfWtanPan = (halfW * (tan (abs pan)))
       ; short = ((halfW, halfH - halfWtanPan), (halfW, 0 - (halfH - halfWtanPan)))
       ; posVertices = [(fst short), (snd short), (fst long), (snd long)]
       ; vertices = if pan >= 0 then posVertices else (invertPoly posVertices)
       } in
   polygon vertices center

generateCorridor w h scale pan center =
   let (nw,nh) = scaleView (w,h) scale in
   if area (nw,nh) > 1 then
      let { c = fromMaybe ((nw / 2),(nh / 2)) center
          ; (_,_,corridor) = generateCorridor nw nh scale pan (Just c) } in
      (nw,nh,[ textured webcamFrame (generateTrapezoid nw nh c pan) ] ++ corridor)
   else (nw,nh,[])

scene (w,h) cpos =
   let (nw,nh,corridor) = generateCorridor w h (fst cpos) (snd cpos) Nothing in
   container w h middle $ collage nw nh corridor

view = lift2 scene Window.dimensions cameraPosition
main = lift view
