module STime exposing (..)

type STime = ST {seconds : Int, milliseconds : Float}

fromString : String -> Maybe STime
fromString str =
  let
    (wholePart, fracPart) =
      case String.split "." str of
        [w, f] -> (w, f)
        [w] -> (w, "0")
  in
    String.toInt wholePart |> Maybe.andThen (\w ->
    String.toInt fracPart |> Maybe.andThen (\f ->
      {seconds = w, milliseconds = 1000.0 * toFloat f}))

toString : STime -> String
toString st =
  String.fromInt st.seconds ++
  (if st.milliseconds == 0.0 then "" else "." ++
