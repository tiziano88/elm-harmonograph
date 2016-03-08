import Array
import Color exposing (..)
import Color.Convert exposing (hexToColor)
import Effects exposing (Effects)
import Focus
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import History
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Signal.Extra
import StartApp
import String
import Task
import Time

import Proto.Harmonograph exposing (..)


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


fps : Int
fps =
  10


maxPhase : Float
maxPhase =
  2 * pi

type alias Model =
  { config : Config
  , time : Float
  , updated : Bool
  , eff : Float
  }


animate : Signal.Mailbox Bool
animate =
  Signal.mailbox False


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs =
      [ Signal.Extra.keepWhen animate.signal 0.0 (Time.fps fps) |> Signal.map (always Tick)
      , History.hash |> Signal.dropRepeats |> Signal.map Url
      ]
    }


init : (Model, Effects Action)
init =
  noEffects initialModel

buttonStyle : Attribute
buttonStyle =
  style
    [ "background-color" => "red"
    , "color" => "white"
    , "font-weight" => "bold"
    , "border" => "none"
    , "border-radius" => "0.3em"
    , "margin" => "0.5em"
    , "padding" => "1em"
    , "cursor" => "pointer"
    ]


initialModel : Model
initialModel =
  { time = 0.0
  , eff = 0.0
  , updated = False
  , config =
    { resolution = 4
    , max = 1000
    , x1 =
      Just
        { frequency = 0.4
        , phase = 0
        , amplitude = 200
        , damping = 0.002
        }
    , x2 =
      Just
        { frequency = 0.8
        , phase = 0
        , amplitude = 200
        , damping = 0.00012
        }
    , y1 =
      Just
        { frequency = 0.3
        , phase = 0
        , amplitude = 230
        , damping = 0.00017
        }
    , y2 =
      Just
        { frequency = 0.4
        , phase = 0
        , amplitude = 200
        , damping = 0.00013
        }
    , x =
        [ { frequency = 0.1
          , phase = 0
          , amplitude = 111
          , damping = 0.001
          }
        , { frequency = 0.2
          , phase = 0
          , amplitude = 222
          , damping = 0.002
          }
        ]
    , y =
        [ { frequency = 0.1
          , phase = 0
          , amplitude = 111
          , damping = 0.001
          }
        ]
    , startColor = "#0000ff"
    , endColor = "#ff0000"
    }
  }


lfo : Model -> Model
lfo model =
  { model | eff = (def model.config.x1).phase + (10 * sin (model.time / (toFloat fps))) }


phaseAdd : Float -> Float -> Float
phaseAdd x y =
  let
    z = x + y
  in
    if
      z > maxPhase
    then
      z - maxPhase
    else
      z


updateUrl : Config -> Effects Action
updateUrl config =
  configEncoder config
    |> JE.encode 0
    |> Http.uriEncode
    |> \s -> "#" ++ s
    |> History.replacePath
    |> Effects.task
    |> Effects.map (\_ -> Tick)


-- Experimental Focus stuff.

con : Focus.Focus Model Config
con =
  Focus.create
    (\m -> m.config)
    (\f m -> {m | config = f m.config})

x1 : Focus.Focus Config (Maybe Params)
x1 =
  Focus.create
    (\c -> c.x1)
    (\f c -> {c | x1 = f c.x1})

x2 : Focus.Focus Config (Maybe Params)
x2 =
  Focus.create
    (\c -> c.x2)
    (\f c -> {c | x2 = f c.x2})

y1 : Focus.Focus Config (Maybe Params)
y1 =
  Focus.create
    (\c -> c.y1)
    (\f c -> {c | y1 = f c.y1})

y2 : Focus.Focus Config (Maybe Params)
y2 =
  Focus.create
    (\c -> c.y2)
    (\f c -> {c | y2 = f c.y2})


type Action
  = Tick
  | Url String
  | SetConfig Config
  | SetStartColor String
  | SetEndColor String
  | X1 Params
  | X2 Params
  | Y1 Params
  | Y2 Params


noEffects : a -> (a, Effects b)
noEffects m =
  (m, Effects.none)


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Tick ->
      let
        c = model.config
        x1 = def model.config.x1
        x2 = def model.config.x2
        y1 = def model.config.y1
        y2 = def model.config.y2
        c1 =
          { c
          | x1 = Just { x1 | phase = phaseAdd x1.phase 0.01 }
          , x2 = Just { x2 | phase = phaseAdd x2.phase 0.01 }
          , y1 = Just { y1 | phase = phaseAdd y1.phase 0.01 }
          , y2 = Just { y2 | phase = phaseAdd y2.phase 0.01 }
          }
      in
        noEffects { model | config = c1 }

    SetConfig c ->
      noEffects { model | config = c }

    Url s ->
      if
        model.updated
      then
        noEffects model
      else
        let
          r =
            Debug.log "url change" s
            |> String.dropLeft 1 -- Remove leading '#'.
            |> Http.uriDecode
            |> JD.decodeString configDecoder
        in
          case r of
            Ok v ->
              noEffects { model | config = v, updated = True }

            Err e ->
              noEffects model

    SetStartColor v ->
      let
        c = model.config
        c1 = { c | startColor = v }
      in
        ( { model | config = c1 }, updateUrl c1 )

    SetEndColor v ->
      let
        c = model.config
        c1 = { c | endColor = v }
      in
        ( { model | config = c1 }, updateUrl c1 )

    X1 p ->
      let
        c = model.config
        c1 = { c | x1 = Just p }
      in
        ( { model | config = c1 }, updateUrl c1 )

    X2 p ->
      let
        c = model.config
      in
        ( { model | config = { c | x2 = Just p } }, updateUrl c )

    Y1 p ->
      let
        c = model.config
      in
        ( { model | config = { c | y1 = Just p } }, updateUrl c )

    Y2 p ->
      let
        c = model.config
      in
        ( { model | config = { c | y2 = Just p } }, updateUrl c )


view address model =
  div
    [ style
      [ "display" => "flex"
      ]
    ]
    [ div []
      [ fromElement <| collage 1000 1000
        [ trace model
        ]
      ]
    , paramControls address model
    , dataWidget address model
    ]

dataWidget address model =
  div []
    [ textarea
      [ style
        [ "width" => "30em"
        , "height" => "60em"
        ]
      , value <| JE.encode 2 <| configEncoder model.config
      ] []
    , button [ buttonStyle ]
      [ Html.text "Load" ]
    ]


paramControls : Signal.Address Action -> Model -> Html
paramControls address model =
  div []
    [ slider
      { title = "resolution"
      , min = 0.0
      , max = 10.0
      , step = 1.0
      , update = \x -> Signal.message address (SetConfig <| let c = model.config in { c | resolution = round x })
      } (toFloat model.config.resolution)
    , slider
      { title = "samples"
      , min = 100.0
      , max = 10000.0
      , step = 100.0
      , update = \x -> Signal.message address (SetConfig <| let c = model.config in { c | max = round x })
      } (toFloat model.config.max)
    , colorPicker (Signal.forwardTo address SetStartColor) (model.config.startColor)
    , Html.text model.config.startColor
    , colorPicker (Signal.forwardTo address SetEndColor) (model.config.endColor)
    , Html.text model.config.endColor
    , Html.text "x1"
    , controlBlock (Signal.forwardTo address X1) (def model.config.x1)
    , Html.text "x2"
    , controlBlock (Signal.forwardTo address X2) (def model.config.x2)
    , Html.text "y1"
    , controlBlock (Signal.forwardTo address Y1) (def model.config.y1)
    , Html.text "y2"
    , controlBlock (Signal.forwardTo address Y2) (def model.config.y2)
    , button [ buttonStyle, onClick animate.address True ] [ Html.text ">" ]
    , button [ buttonStyle, onClick animate.address False ] [ Html.text "||" ]
    ]


(=>) : String -> String -> (String, String)
(=>) = (,)


controlBlock : Signal.Address Params -> Params -> Html
controlBlock address p =
  div []
    [ slider
      { title = "amplitude"
      , min = 0.0
      , max = 1000.0
      , step = 1.0
      , update = \x -> Signal.message address ({ p | amplitude = x })
      } p.amplitude
    , slider
      { title = "phase"
      , min = 0.0
      , max = maxPhase
      , step = 0.001
      , update = \x -> Signal.message address ({ p | phase = x })
      } p.phase
    , slider
      { title = "frequency"
      , min = 0.0
      , max = 10.0
      , step = 0.01
      , update = \x -> Signal.message address ({ p | frequency = x })
      } p.frequency
    , slider
      { title = "damping"
      , min = 0.0
      , max = 0.003
      , step = 0.000001
      , update = \x -> Signal.message address ({ p | damping = x })
      } p.damping
    ]


colorPicker : Signal.Address String -> String -> Html
colorPicker address v =
  input
    [ type' "color"
    , Html.Attributes.value v
    , on "input" targetValue (Signal.message address)
    , style
      [ "width" => "30em"
      ]
    ] []


type alias SliderAttributes =
  { title : String
  , min : Float
  , max : Float
  , step : Float
  , update : Float -> Signal.Message
  }


slider : SliderAttributes -> Float -> Html
slider attr v =
  div
    [ style
      [ "width" => "40em"
      ]
    ]
    [ span
      [ style
        [ "display" => "inline-block"
        , "width" => "7em"
        ]
      ]
      [ Html.text attr.title
      ]
    , input
      [ type' "range"
      , Html.Attributes.min <| toString attr.min
      , Html.Attributes.max <| toString attr.max
      , Html.Attributes.step <| toString attr.step
      , Html.Attributes.value <| toString v
      , on "input" targetValue (parseFloat >> attr.update)
      , style
        [ "width" => "30em"
        ]
      ] []
    , span
      [ style
        [ "display" => "inline-block"
        , "width" => "7em"
        ]
      ]
      [ Html.text <| toString v
      ]
    ]


parseFloat : String -> Float
parseFloat s =
  String.toFloat s |> Result.withDefault 0


decodeColor : String -> Color
decodeColor s =
  hexToColor s
  |> Maybe.withDefault black


trace : Model -> Form
trace model =
  traced (solid (decodeColor model.config.startColor)) (path <| values model)


values : Model -> List (Float, Float)
values model =
  let
    res = model.config.resolution
    n = model.config.max
  in
    Array.initialize (n*res) (\x -> point model ((toFloat x)/(toFloat res))) |> Array.toList


point : Model -> Float -> (Float, Float)
point model time =
  ( List.sum
    [ eval (def model.config.x1) time
    , eval (def model.config.x2) time
    ]
  , List.sum
    [ eval (def model.config.y1) time
    , eval (def model.config.y2) time
    ]
  )


def : Maybe Params -> Params
def x =
  case x of
    Just x ->
      x

    Nothing ->
      { amplitude = 0.0
      , damping = 0.0
      , frequency = 0.0
      , phase = 0.0
      }


eval : Params -> Float -> Float
eval p t =
  p.amplitude * sin (t * p.frequency + p.phase) * e ^ (-p.damping * t)
