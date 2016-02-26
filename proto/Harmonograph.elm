module Harmonograph where


import Json.Decode as JD exposing ((:=))
import Json.Encode as JE


optional : JD.Decoder a -> JD.Decoder (Maybe a)
optional decoder =
  JD.oneOf
    [ JD.map Just decoder
    , JD.succeed Nothing
    ]


withDefault : a -> JD.Decoder a -> JD.Decoder a
withDefault default decoder =
  JD.oneOf
    [ decoder
    , JD.succeed default
    ]


intField : String -> JD.Decoder Int
intField name =
  withDefault 0 (name := JD.int)


floatField : String -> JD.Decoder Float
floatField name =
  withDefault 0.0 (name := JD.float)


boolField : String -> JD.Decoder Bool
boolField name =
  withDefault False (name := JD.bool)


stringField : String -> JD.Decoder String
stringField name =
  withDefault "" (name := JD.string)


messageField : JD.Decoder a -> String -> JD.Decoder (Maybe a)
messageField decoder name =
  optional (name := decoder)


enumField : JD.Decoder a -> String -> JD.Decoder a
enumField decoder name =
  (name := decoder)


optionalEncoder : (a -> JE.Value) -> Maybe a -> JE.Value
optionalEncoder encoder v =
  case v of
    Just x ->
      encoder x
    
    Nothing ->
      JE.null


type alias Params =
  { frequency : Float
  , phase : Float
  , amplitude : Float
  , damping : Float
  }


paramsDecoder : JD.Decoder Params
paramsDecoder =
  JD.object4 Params
    (floatField "frequency")
    (floatField "phase")
    (floatField "amplitude")
    (floatField "damping")


paramsEncoder : Params -> JE.Value
paramsEncoder v =
  JE.object
    [ ("frequency", JE.float v.frequency)
    , ("phase", JE.float v.phase)
    , ("amplitude", JE.float v.amplitude)
    , ("damping", JE.float v.damping)
    ]


type alias Model =
  { x1 : Maybe Params
  , x2 : Maybe Params
  , y1 : Maybe Params
  , y2 : Maybe Params
  }


modelDecoder : JD.Decoder Model
modelDecoder =
  JD.object4 Model
    (messageField paramsDecoder "x1")
    (messageField paramsDecoder "x2")
    (messageField paramsDecoder "y1")
    (messageField paramsDecoder "y2")


modelEncoder : Model -> JE.Value
modelEncoder v =
  JE.object
    [ ("x1", optionalEncoder paramsEncoder v.x1)
    , ("x2", optionalEncoder paramsEncoder v.x2)
    , ("y1", optionalEncoder paramsEncoder v.y1)
    , ("y2", optionalEncoder paramsEncoder v.y2)
    ]


