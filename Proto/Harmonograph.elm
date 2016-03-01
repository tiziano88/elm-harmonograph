module Proto.Harmonograph where


import Json.Decode as JD exposing ((:=))
import Json.Encode as JE


optionalDecoder : JD.Decoder a -> JD.Decoder (Maybe a)
optionalDecoder decoder =
  JD.oneOf
    [ JD.map Just decoder
    , JD.succeed Nothing
    ]


optionalFieldDecoder : JD.Decoder a -> String -> JD.Decoder (Maybe a)
optionalFieldDecoder decoder name =
  optionalDecoder (name := decoder)


repeatedFieldDecoder : JD.Decoder a -> String -> JD.Decoder (List a)
repeatedFieldDecoder decoder name =
  JD.list (name := decoder)


withDefault : a -> JD.Decoder a -> JD.Decoder a
withDefault default decoder =
  JD.oneOf
    [ decoder
    , JD.succeed default
    ]


intFieldDecoder : String -> JD.Decoder Int
intFieldDecoder name =
  withDefault 0 (name := JD.int)


floatFieldDecoder : String -> JD.Decoder Float
floatFieldDecoder name =
  withDefault 0.0 (name := JD.float)


boolFieldDecoder : String -> JD.Decoder Bool
boolFieldDecoder name =
  withDefault False (name := JD.bool)


stringFieldDecoder : String -> JD.Decoder String
stringFieldDecoder name =
  withDefault "" (name := JD.string)


enumFieldDecoder : JD.Decoder a -> String -> JD.Decoder a
enumFieldDecoder decoder name =
  (name := decoder)


optionalEncoder : (a -> JE.Value) -> Maybe a -> JE.Value
optionalEncoder encoder v =
  case v of
    Just x ->
      encoder x
    
    Nothing ->
      JE.null


repeatedFieldEncoder : (a -> JE.Value) -> List a -> JE.Value
repeatedFieldEncoder encoder v =
  JE.list <| List.map encoder v


type alias Param =
  { value : Float -- 1
  , frequency : Float -- 2
  , amplitude : Float -- 3
  }


paramDecoder : JD.Decoder Param
paramDecoder =
  JD.object3 Param
    (floatFieldDecoder "value")
    (floatFieldDecoder "frequency")
    (floatFieldDecoder "amplitude")


paramEncoder : Param -> JE.Value
paramEncoder v =
  JE.object
    [ ("value", JE.float v.value)
    , ("frequency", JE.float v.frequency)
    , ("amplitude", JE.float v.amplitude)
    ]


type alias Params =
  { frequency : Float -- 1
  , phase : Float -- 2
  , amplitude : Float -- 3
  , damping : Float -- 4
  }


paramsDecoder : JD.Decoder Params
paramsDecoder =
  JD.object4 Params
    (floatFieldDecoder "frequency")
    (floatFieldDecoder "phase")
    (floatFieldDecoder "amplitude")
    (floatFieldDecoder "damping")


paramsEncoder : Params -> JE.Value
paramsEncoder v =
  JE.object
    [ ("frequency", JE.float v.frequency)
    , ("phase", JE.float v.phase)
    , ("amplitude", JE.float v.amplitude)
    , ("damping", JE.float v.damping)
    ]


type alias Config =
  { resolution : Int -- 1
  , max : Int -- 2
  , x1 : Maybe Params -- 3
  , x2 : Maybe Params -- 4
  , y1 : Maybe Params -- 5
  , y2 : Maybe Params -- 6
  , x : List Params -- 7
  , y : List Params -- 8
  }


configDecoder : JD.Decoder Config
configDecoder =
  JD.object8 Config
    (intFieldDecoder "resolution")
    (intFieldDecoder "max")
    (optionalFieldDecoder paramsDecoder "x1")
    (optionalFieldDecoder paramsDecoder "x2")
    (optionalFieldDecoder paramsDecoder "y1")
    (optionalFieldDecoder paramsDecoder "y2")
    (repeatedFieldDecoder paramsDecoder "x")
    (repeatedFieldDecoder paramsDecoder "y")


configEncoder : Config -> JE.Value
configEncoder v =
  JE.object
    [ ("resolution", JE.int v.resolution)
    , ("max", JE.int v.max)
    , ("x1", optionalEncoder paramsEncoder v.x1)
    , ("x2", optionalEncoder paramsEncoder v.x2)
    , ("y1", optionalEncoder paramsEncoder v.y1)
    , ("y2", optionalEncoder paramsEncoder v.y2)
    , ("x", repeatedFieldEncoder paramsEncoder v.x)
    , ("y", repeatedFieldEncoder paramsEncoder v.y)
    ]


