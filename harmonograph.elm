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

import Harmonograph exposing (..)


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


fps : Int
fps =
  100


type alias Model =
  { config : Config
  , time : Float
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
  , config =
    { resolution = 4
    , max = 1000
    , x1 =
      Just
        { frequency = 0.7
        , phase = 0
        , amplitude = 200
        , damping = 0.002
        }
    , x2 =
      Just
        { frequency = 0.6
        , phase = 0
        , amplitude = 200
        , damping = 0.00012
        }
    , y1 =
      Just
        { frequency = 0.5
        , phase = 0
        , amplitude = 230
        , damping = 0.0000017
        }
    , y2 =
      Just
        { frequency = 0.7
        , phase = 0
        , amplitude = 200
        , damping = 0.0000013
        }
    }
  }


lfo : Model -> Model
lfo model =
  { model | eff = (def model.config.x1).phase + (10 * sin (model.time / (toFloat fps))) }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Tick ->
      ( lfo { model | time = model.time + 1}, Effects.none )

    M f ->
      let
        c = model.config
      in
        ( { model | config = f c }, Effects.none )

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
        p = f <| def c.y1
      in
        ( { model | config = { c | y2 = Just p } }, Effects.none )

type Action
  = Tick
  | M (Config -> Config)
  | X1 (Params -> Params)
  | X2 (Params -> Params)
  | Y1 (Params -> Params)
  | Y2 (Params -> Params)

view address model =
  div []
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
    , slider
      { title = "phase (effective)"
      , min = 0.0
      , max = 10.0
      , step = 0.001
      , update = \x -> Signal.message address (M (\p -> p))
      } model.eff
    , controlBlock (Signal.forwardTo address X1) (def model.config.x1)
    , Html.text "x2"
    , controlBlock (Signal.forwardTo address X2) (def model.config.x2)
    , Html.text "y1"
    , controlBlock (Signal.forwardTo address Y1) (def model.config.y1)
    , Html.text "y2"
    , controlBlock (Signal.forwardTo address Y2) (def model.config.y2)
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
      , max = 10.0
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
  div []
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
      , on "input" targetValue (parse >> attr.update)
      , style
        [ "width" => "30em"
        ]
      ] []
    , Html.text <| toString v
    ]


parse : String -> Float
parse s =
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
    List.map (\x -> point model ((toFloat x)/(toFloat res))) [0..(n*res)]


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
