module Event exposing
  ( Event
  , moment
  , matchMoments
  , time
  , duration
  , proc
  , identifier
  , example
  )

type EventType =
    Duration Float
  | Moment

type alias Event =
  { name : String
  , time : Float
  , proc : Int
  , identifier : Int
  , typ : EventType
  , args : List String
  }

type alias MomentDesc a =
  {a | name: String, time: Float, proc: Int, identifier : Int, args: List String}

moment : MomentDesc a -> Event
moment x =
  {name=x.name, time=x.time, proc=x.proc, typ=Moment, args=x.args, identifier=x.identifier}

matchMoments : MomentDesc a -> MomentDesc b -> Event
matchMoments x y =
  { name = x.name
  , time = x.time
  , proc = x.proc
  , identifier = x.identifier
  , typ = Duration (y.time - x.time)
  , args = x.args ++ y.args
  }

duration : Event -> Float
duration e =
  case e.typ of
    Duration x -> x
    Moment -> 0.0

time : Event -> Float
time e = e.time

proc : Event -> Int
proc e = e.proc

identifier : Event -> Int
identifier e = e.identifier

example : List Event
example =
  [ {name="", args = [], identifier=0, proc = 0, time = 0.000318, typ = Duration 0.0083}
  , {name="", args = [], identifier=1, proc = 3, time = 0.005,    typ = Duration 0.0051}
  , {name="", args = [], identifier=2, proc = 1, time = 0.006,    typ = Moment}
  , {name="", args = [], identifier=3, proc = 2, time = 0.011,    typ = Duration 0.001}
  , {name="", args = [], identifier=4, proc = 0, time = 0.013,    typ = Duration 0.0005}
  ]
