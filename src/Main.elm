module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Html as H exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events as HE exposing (onClick)
import File.Select
import File exposing (File)
import Task
-- import Time

import Keyboard exposing (RawKey, Key(..))

-- My modules
import Event exposing (Event)
import Trace exposing (Trace)
import ParseTrace
-- import SVGExample
-- import MM exposing (Model, Msg, update, init)

main : Program () Model Msg
main =
  Browser.document
  { init = init
  , view = view
  , update = update
  , subscriptions = subs
  }

type Msg =
    FileRequested
  | FileSelected File
  | FileLoaded Trace
  | ScrollPosition Int
  | TraceViewPos Float
  | Info String
  | Zoom Float
  | SetDimensions Int Int
  | FocusEvent Event
  | UnfocusEvent
  | KeyDown (Maybe Key)
  | KeyUp (Maybe Key)

type FileStatus =
    Waiting
  | Loading File
  | Loaded {file : File, trace : Trace}

read : File -> Cmd Msg
read file =
  Task.perform FileLoaded (Task.map ParseTrace.parse <| File.toString file)

updateFileStatus msg fileStatus =
  case msg of
    FileRequested ->
      case fileStatus of
        Loading _ -> (fileStatus, Cmd.none)
        _ -> (fileStatus, File.Select.file ["text/plain"] FileSelected)

    FileSelected ff ->
      (Loading ff, read ff)

    FileLoaded trace ->
      case fileStatus of
        Loading ff -> (Loaded {file=ff, trace=trace}, Cmd.none)
        _ -> (Waiting, Cmd.none)

    _ -> (fileStatus, Cmd.none)

type alias Model =
  { fileStatus : FileStatus
  , focus : Maybe Event
  , scroll : Int
  , traceViewX : Float
  , zoom : Float
  , sceneWidth : Int
  , sceneHeight : Int
  , recentKeyUp : Maybe Key
  , recentKeyDown : Maybe Key
  , info : Maybe String
  }

init () =
  let
    setDimMsg vp = SetDimensions (floor vp.scene.width) (floor vp.scene.height)
    w = Task.perform setDimMsg Browser.Dom.getViewport
    m =
      { fileStatus = Waiting
      , focus = Nothing
      , scroll = 0
      , traceViewX = 0.0
      , zoom = 1.0
      , info = Nothing
      , sceneWidth = 100
      , sceneHeight = 100
      , recentKeyUp = Nothing
      , recentKeyDown = Nothing
      }
  in
    (m, w)

subs model =
  Sub.batch
    [ Browser.Events.onResize SetDimensions
    , Keyboard.downs (\rk -> KeyDown (Keyboard.anyKeyOriginal rk))
    , Keyboard.ups (\rk -> KeyUp (Keyboard.anyKeyOriginal rk))
    ]

zoomer ratio model =
  let
    newZoom = max 1.0 (model.zoom * ratio)
    actualRatio = newZoom / model.zoom

    handleResult r =
      case r of
        Err (Browser.Dom.NotFound s) -> Info ("could not find " ++ s)
        Ok x -> Zoom newZoom

    cmd =
      Browser.Dom.getViewportOf "trace-view"
      |> Task.andThen (\info ->
          let
            y = info.viewport.y
            -- center
            oldCenter = info.viewport.x + info.viewport.width / 2.0
            newCenter = oldCenter * actualRatio
            x = newCenter - info.viewport.width / 2.0
          in
            Browser.Dom.setViewportOf "trace-view" x y
            |> Task.map (\_ -> x))
      |> Task.attempt handleResult
  in
    ({model | zoom = newZoom}, cmd)

update msg model =
  case msg of
    KeyUp mk -> ({model | recentKeyUp = mk}, Cmd.none)
    KeyDown mk ->
      let
        m = {model | recentKeyDown = mk}
      in
        case mk of
          Just ArrowDown -> zoomer (1/1.25) m
          Just ArrowUp -> zoomer 1.25 m
          _ -> (m, Cmd.none)
    Zoom z -> ({model | zoom = z}, Cmd.none)
    TraceViewPos x -> ({model | traceViewX = x}, Cmd.none)
    ScrollPosition x ->
      let
        handleResult r =
          case r of
            Err (Browser.Dom.NotFound _) -> 420.15210
            Ok xx -> xx
        cmd =
          Browser.Dom.getViewportOf "trace-view"
            |> Task.map (\info -> info.viewport.x)
            |> Task.attempt (TraceViewPos << handleResult)
      in
        ({model | scroll = x}, cmd)
    Info str -> ({model | info = Just str}, Cmd.none)
    SetDimensions w h -> ({model | sceneWidth = w, sceneHeight = h}, Cmd.none)
    FocusEvent e -> ({model | focus = Just e}, Cmd.none)
    UnfocusEvent -> ({model | focus = Nothing}, Cmd.none)
    _ ->
      let
        (fs, cmd) = updateFileStatus msg model.fileStatus
      in
        ({model | fileStatus = fs}, cmd)

view model =
  let
    title = "File example"

    thebutton = button [ onClick FileRequested ] [ text "select file" ]

    keyinfo =
      div [] [ text <| "keyup " ++ Debug.toString (model.recentKeyUp)
             , H.br [] []
             , text <| "keydown " ++ Debug.toString (model.recentKeyDown)
             ]

    maintext =
      case model.fileStatus of
        Waiting ->
          div [] [ text "waiting to select file..." ]

        Loading ff ->
          div [] [ text <| "loading file " ++ (File.name ff) ]

        Loaded {file, trace} ->
          div []
            [ text <| "loaded file " ++ (File.name file)
            , H.br [] []
            , text <| "num events: " ++ (String.fromInt (Trace.numEvents trace))
            ]

    scrollPosText =
      div [] [ text <| String.fromInt model.scroll
             , H.br [] []
             , text <| Debug.toString model.traceViewX
             ]

    infoText =
      div [] [ text <| Maybe.withDefault "---" model.info ]

    traceViewSettings =
      { width = model.sceneWidth-15
      , height = model.sceneHeight-250
      , zoom = model.zoom
      , start = model.scroll
      , focus = model.focus
      , scrollMsg = ScrollPosition
      , zoomMsg = Zoom
      , mouseOverMsg = FocusEvent
      , mouseOutMsg = UnfocusEvent
      }

    eventInfo =
      div [] [ text <| Debug.toString (model.focus) ]

    -- slider =
    --   H.input
    --     [ HA.type_ "range"
    --     , HA.min "1.0"
    --     , HA.max "10000.0"
    --     , HA.style "width" "90%"
    --     , HA.value <| String.fromFloat (model.zoom * 100.0)
    --     , HE.onInput (\s ->
    --         (Maybe.withDefault 1.0 (String.toFloat s)) / 100.0
    --         |> Zoom)
    --     ]
    --     []

    traceView =
      case model.fileStatus of
        Loaded t -> [Trace.view traceViewSettings t.trace, eventInfo] --, slider]
        _ -> []

    body = [thebutton, keyinfo, maintext, scrollPosText, infoText] ++ traceView
  in
    {title=title, body=body}

