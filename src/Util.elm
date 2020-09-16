module Util exposing
  ( nearestLargerPow10
  , rightStrip
  , timeToString
  )

import Round

nearestLargerPow10 : Float -> Float
nearestLargerPow10 x =
  if x == 0.0 then x else
  if x < 0.0 then -1.0 * nearestLargerPow10 (-x) else
  let
    loopSmaller p =
      if p * 0.1 < x
      then p
      else loopSmaller (p * 0.1)

    loopLarger p =
      if p >= x
      then p
      else loopLarger (p * 10.0)
  in
    if x < 1.0
    then loopSmaller 1.0
    else loopLarger 1.0

rightStrip : String -> String -> String
rightStrip pattern str =
  if String.length pattern == 0 then
    str
  else if String.endsWith pattern str then
    rightStrip pattern (String.dropRight (String.length pattern) str)
  else
    str

leftStrip : String -> String -> String
leftStrip pattern str =
  if String.length pattern == 0 then
    str
  else if String.startsWith pattern str then
    leftStrip pattern (String.dropLeft (String.length pattern) str)
  else
    str

timeToString : Float -> String
timeToString t =
  if t < 0.0 then "-" ++ timeToString (-t) else
  let
    fmt = Round.round 3 >> rightStrip "0" >> leftStrip "0" >> rightStrip "."
  in
    if t == 0.0 then
      "0s"
    else if t < 0.0001 then
      fmt (t * 1000000.0) ++ "us"
    else if t < 0.1 then
      fmt (t * 1000.0) ++ "ms"
    else
      fmt t ++ "s"
