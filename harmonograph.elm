import Array
import Color exposing (..)
import Effects exposing (Effects)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as JE
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
  , animate : Bool
  , eff : Float
  }


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [ Time.fps fps |> Signal.map (\_ -> Tick) ]
    }


init : (Model, Effects Action)
init = (initialModel, Effects.none)


initialModel : Model
initialModel =
  { time = 0.0
  , eff = 0.0
  , animate = False
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


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Tick ->
      if
        model.animate
      then
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
          ({ model | config = c1 }, Effects.none)
      else
        (model, Effects.none)

    M f ->
      let
        c = model.config
      in
        ( { model | config = f c }, Effects.none )

    Start ->
      ( { model | animate = True }, Effects.none )

    Stop ->
      ( { model | animate = False }, Effects.none )

    X1 f ->
      let
        c = model.config
        p = f <| def c.x1
      in
        ( { model | config = { c | x1 = Just p } }, Effects.none )

    X2 f ->
      let
        c = model.config
        p = f <| def c.x2
      in
        ( { model | config = { c | x2 = Just p } }, Effects.none )

    Y1 f ->
      let
        c = model.config
        p = f <| def c.y1
      in
        ( { model | config = { c | y1 = Just p } }, Effects.none )

    Y2 f ->
      let
        c = model.config
        p = f <| def c.y2
      in
        ( { model | config = { c | y2 = Just p } }, Effects.none )

type Action
  = Tick
  | Start
  | Stop
  | M (Config -> Config)
  | X1 (Params -> Params)
  | X2 (Params -> Params)
  | Y1 (Params -> Params)
  | Y2 (Params -> Params)

view address model =
  div
    [ style
      [ "display" => "flex"
      ]
    ]
    [ fromElement <| collage 1000 1000
      [ trace model
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
    , button []
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
      , update = \x -> Signal.message address (M (\p -> { p | resolution = round x }))
      } (toFloat model.config.resolution)
    , slider
      { title = "samples"
      , min = 100.0
      , max = 10000.0
      , step = 100.0
      , update = \x -> Signal.message address (M (\p -> { p | max = round x }))
      } (toFloat model.config.max)
    , Html.text "x1"
    , controlBlock (Signal.forwardTo address X1) (def model.config.x1)
    , Html.text "x2"
    , controlBlock (Signal.forwardTo address X2) (def model.config.x2)
    , Html.text "y1"
    , controlBlock (Signal.forwardTo address Y1) (def model.config.y1)
    , Html.text "y2"
    , controlBlock (Signal.forwardTo address Y2) (def model.config.y2)
    , button [ onClick address Start ] [ Html.text ">" ]
    , button [ onClick address Stop ] [ Html.text "||" ]
    ]


(=>) : String -> String -> (String, String)
(=>) = (,)


controlBlock : Signal.Address (Params -> Params) -> Params -> Html
controlBlock address p =
  div []
    [ slider
      { title = "amplitude"
      , min = 0.0
      , max = 1000.0
      , step = 1.0
      , update = \x -> Signal.message address (\p -> { p | amplitude = x })
      } p.amplitude
    , slider
      { title = "phase"
      , min = 0.0
      , max = maxPhase
      , step = 0.001
      , update = \x -> Signal.message address (\p -> { p | phase = x })
      } p.phase
    , slider
      { title = "frequency"
      , min = 0.0
      , max = 10.0
      , step = 0.01
      , update = \x -> Signal.message address (\p -> { p | frequency = x })
      } p.frequency
    , slider
      { title = "damping"
      , min = 0.0
      , max = 0.003
      , step = 0.000001
      , update = \x -> Signal.message address (\p -> { p | damping = x })
      } p.damping
    ]


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


trace : Model -> Form
trace model =
  traced (solid blue) (path <| values model)


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
