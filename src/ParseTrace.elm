module ParseTrace exposing (parse)

import Dict

import Trace exposing (Trace)
import Event exposing (Event)

type MaybeBeginEnd = Begin | End | Moment

type alias ParsedLine =
  {name: String, typ: MaybeBeginEnd, proc: Int, identifier: Int, time: Float, args: List String}

parseLine : Int -> String -> Maybe ParsedLine
parseLine idx str =
  case String.split "," str of
    name :: procStr :: timeStr :: args ->
      String.toInt procStr |> Maybe.andThen (\proc ->
      String.toFloat timeStr |> Maybe.andThen (\time ->
      let
        (nn, typ) =
          if String.endsWith "_ENTER" name then
            (String.dropRight 6 name, Begin)
          else if String.endsWith "_LEAVE" name then
            (String.dropRight 6 name, End)
          else
            (name, Moment)
      in
        Just {name=nn, typ=typ, proc=proc, identifier=idx, time=time, args=args}))

    _ -> Nothing

-- recorded events can come in pairs FOO_ENTER and FOO_LEAVE, and we want to
-- match these as a single event FOO with the appropriate duration.
-- any unmatched events are returned as the second result
matchEvents : List ParsedLine -> (List Event, List ParsedLine)
matchEvents lines =
  let
    unravel d = List.concat (Dict.values d)

    push line d =
      Dict.update
        (line.proc, line.name)
        (\previous -> Just (line :: Maybe.withDefault [] previous))
        d

    pop (proc, name) d =
      case Dict.get (proc, name) d of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (line :: rest) -> Just (line, Dict.insert (proc, name) rest d)

    -- 'open' is a dict of type {(proc, name) -> List ParsedLine}}
    loop unmatched matched open remaining =
      case remaining of
        [] -> (matched, unmatched ++ unravel open)
        line :: rest ->
          case line.typ of
            Moment ->
              loop unmatched (Event.moment line :: matched) open rest
            Begin ->
              loop unmatched matched (push line open) rest
            End ->
              case pop (line.proc, line.name) open of
                Nothing ->
                  loop (line :: unmatched) matched open rest
                Just (partner, restOpen) ->
                  loop unmatched (Event.matchMoments partner line :: matched) restOpen rest
  in
    loop [] [] Dict.empty (List.sortBy .time lines)

parse : String -> Trace
parse input =
  let
    lines = String.lines input

    -- this also throws out any lines that failed to parse
    parsedLines =
      lines                         -- List String
      |> List.indexedMap parseLine  -- List (Maybe ParsedLine)
      |> List.filterMap (\x -> x)   -- List ParsedLine

    -- does also give us back the unmatched lines, but we'll just ignore for now
    (events, _) = matchEvents parsedLines
  in
    Trace.fromEventList events
