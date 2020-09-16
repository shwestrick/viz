module Trace exposing (Trace, view, fromEventList, numEvents)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events as SE
import Json.Decode as JD

import Dict exposing (Dict)

import Util
import Event exposing (Event)

-- events ordered by their start time (seconds)
type alias Trace =
  { numProcs : Int
  , events : Dict Float Event
  }

fromEventList : List Event -> Trace
fromEventList es =
  let
    events = Dict.fromList <| List.map (\e -> (e.time, e)) es
    maxProc = List.foldl Basics.max 0 (List.map .proc es)
  in
    { numProcs = maxProc+1, events = events}

timeBounds : Trace -> (Float, Float)
timeBounds trace =
  let
    last = Dict.foldl (\k e b -> Basics.max (k + Event.duration e) b) 0.0 trace.events
    first = Dict.foldl (\k e b -> Basics.min k b) last trace.events
  in
    (first, last)

numEvents : Trace -> Int
numEvents trace =
  Dict.size trace.events

{------------------ ViewSettings Fields ---------------------------------------
|
|  width, height:
|    desired size, in pixels. TODO height is currently not used...
|
|  start:
|    The current "scroll x" position. Not used right now but eventually can use
|    to cull the rendered events so that we don't waste time on events that
|    can't be seen right now.
|    Should eventually extend this to vertical scrolling too
|
|  zoom:
|    The horizontal zooming factor, must be at least 1.
|    When zoom is 1, we see the entire trace within the given width.
|
|  focus:
|    The identifier of the event the mouse is currently on top of, if any
|
|  scrollMsg x:
|    signal that left-right scrolling is anchored at pixel x
|
|  mouseOverMsg x:
|    the mouse is now on top of event x (event identifier)
|
|  mouseOutMsg:
|    the mouse is no longer on top of an event
------------------------------------------------------------------------------}
type alias ViewSettings msg =
  { width : Int
  , height : Int
  , start : Int
  , zoom : Float
  , focus : Maybe Event
  , scrollMsg : Int -> msg
  , zoomMsg : Float -> msg
  , mouseOverMsg : Event -> msg
  , mouseOutMsg : msg
  }

box : msg -> msg -> Bool -> (Int, Int) -> (Int, Int) -> Svg msg
box mouseOverMsg mouseOutMsg isFocused (x, y) (w, h) =
  rect
    [ SA.x <| String.fromInt x
    , SA.y <| String.fromInt y
    , SA.width <| String.fromInt <| Basics.max w 1
    , SA.height <| String.fromInt <| Basics.max h 1
    , rx "1"
    , ry "1"
    , fillOpacity "0.5"
    , fill (if isFocused then "blue" else "grey")
    , stroke (if isFocused then "gold" else "black")
    -- , strokeDasharray "2,2"
    , strokeWidth (if isFocused then "2" else "1")
    , SE.onMouseOver mouseOverMsg
    , SE.onMouseOut mouseOutMsg
    ]
    []

view : ViewSettings msg -> Trace -> Html msg
view settings trace =
  let
    (firstEventTime, lastEventTime) = timeBounds trace
    overallEventDuration = lastEventTime - firstEventTime

    timePadding = overallEventDuration * 0.01
    startTime = firstEventTime - timePadding
    endTime = lastEventTime + timePadding
    overallDuration = endTime - startTime

    -- convert a duration to a number of pixels
    timepx t =
      (t / overallDuration) * toFloat settings.width * settings.zoom |> round

    -- convert a number of pixels to a duration
    pxtime px =
      (toFloat px / (settings.zoom * toFloat settings.width))
      * overallDuration

    topPadding = 25 + 15
    botPadding = 30
    betweenPadding = 10
    totalBetweenPadding = (trace.numProcs-1) * betweenPadding
    totalPadding = topPadding + botPadding + totalBetweenPadding
    oneProcHeight =
      Basics.max 15.0 <|
      Basics.min 80.0 <|
        ((toFloat <| settings.height - totalPadding) / toFloat trace.numProcs)
    totalHeight = totalPadding + round (oneProcHeight * toFloat trace.numProcs)
    procStartY p =
      topPadding + round (toFloat p * (oneProcHeight + toFloat betweenPadding))

    isNotVisible x w =
      (x+w < settings.start) || (x > settings.start + settings.width)

    isNotVisibleCoordsDims (x, y) (w, h) = isNotVisible x w

    viewEvent t e =
      let
        -- (x, y)
        coords = (timepx (t - startTime), procStartY (Event.proc e))
        -- (width, height)
        dims = (timepx (Event.duration e), round oneProcHeight)
        isFocused =
          case settings.focus of
            Nothing -> False
            Just event -> Event.identifier e == Event.identifier event
        mouseOverMsg = settings.mouseOverMsg e
      in
        if isNotVisibleCoordsDims coords dims then
          Nothing
        else
          Just (box mouseOverMsg settings.mouseOutMsg isFocused coords dims)

    boxes =
      Dict.toList trace.events
      |> List.filterMap (\(t, e) -> viewEvent t e)

    timeline i interval majorspace =
      let
        t = toFloat i * interval
        px = timepx (t - startTime)
        isMajor = (0 == modBy majorspace i)
        tLastMajor = t - toFloat (remainderBy majorspace i) * interval
      in
        if isNotVisible px 0 then
          []
        else
          [ S.line
              [ x1 <| String.fromInt px
              , x2 <| String.fromInt px
              , y1 (if isMajor then "15" else "30")
              , y2 <| String.fromInt <| totalHeight - botPadding + 10
              , strokeWidth (if isMajor then "2" else "1")
              , stroke (if isMajor then "red" else "gray")
              ] []
          , S.text_
              [ x <| String.fromInt px
              , y (if isMajor then "12" else "27")
              , fontSize (if isMajor then "14" else "12")
              , fill (if isMajor then "red" else "gray")
              , textAnchor "middle"
              ]
              [ S.text <|
                if isMajor then
                  Util.timeToString (t - firstEventTime)
                else
                  "+" ++ Util.timeToString (interval * toFloat (remainderBy majorspace i))
              ]
          ]

    horizontalDelimiter i =
      let
        y = procStartY (i+1) - betweenPadding // 2
      in
        S.line
          [ x1 "0"
          , x2 <| String.fromInt <| round <| toFloat settings.width * settings.zoom
          , y1 <| String.fromInt y
          , y2 <| String.fromInt y
          , strokeWidth "3"
          , stroke "gray"
          -- , strokeDasharray "5,5"
          ] []
        -- , S.line
        --     [ x1 "0"
        --     , x2 <| String.fromInt settings.width
        --     , y1 <| String.fromInt y
        --     , y2 <| String.fromInt y
        --     , strokeWidth "1"
        --     , stroke "black"
        --     ] []
        -- ]

    -- How many pixels apart should adjacent time markers be placed?
    desiredLineSpacing = 40

    desiredInterval = pxtime desiredLineSpacing
    desiredMajorInterval = Util.nearestLargerPow10 (4.0 * desiredInterval)
    (minorInterval, majorSpacing) =
      if desiredMajorInterval / 8.0 >= desiredInterval then
        (desiredMajorInterval / 8.0, 8)
      else
        (desiredMajorInterval / 4.0, 4)
    numTimeLines = majorSpacing + ceiling(overallDuration / minorInterval)
    firstTimeLine = floor(startTime / minorInterval)

    timeLines =
      List.range firstTimeLine (firstTimeLine + numTimeLines)
      |> List.map (\i -> timeline i minorInterval majorSpacing)
      |> List.concat

    horizontalDelimiters =
      if trace.numProcs == 1 then [] else
      List.range 0 (trace.numProcs - 2)
      |> List.map horizontalDelimiter

    visuals =
      svg
        [ SA.width <| String.fromInt <| round (settings.zoom * toFloat settings.width)
        , SA.height <| String.fromInt <| totalHeight + 1
        ]
        (timeLines ++ horizontalDelimiters ++ boxes)
  in
    H.div
      [ HA.id "trace-view"
      , HA.style "overflow" "scroll"
      , HA.style "width" (String.fromInt settings.width ++ "px")
      , HA.style "border" "2px dotted"
      , HE.on "scroll"
          ( JD.map settings.scrollMsg
              (JD.at ["target","scrollLeft"] JD.int)
              -- (JD.at ["target","scrollTop"] JD.float)
          )
      ]
      [ visuals
      ]
