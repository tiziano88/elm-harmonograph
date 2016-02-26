import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp
import String


main : Signal Html
main =
  StartApp.start { model = initialModel, view = view, update = update }


type alias Params =
  { frequency : Float
  , phase : Float
  , amplitude : Float
  , damping : Float
  }


type alias Model =
  { px1 : Params
  , px2 : Params
  , py1 : Params
  , py2 : Params
  }


initialModel =
  { px1 =
    { frequency = 0.7
    , phase = 0
    , amplitude = 200
    , damping = 0.002
    }
  , px2 =
    { frequency = 0.6
    , phase = 0
    , amplitude = 200
    , damping = 0.00012
    }
  , py1 =
    { frequency = 0.5
    , phase = 0
    , amplitude = 230
    , damping = 0.0000017
    }
  , py2 =
    { frequency = 0.7
    , phase = 0
    , amplitude = 200
    , damping = 0.0000013
    }
  }


update action model =
  case action of
    X1 f ->
      let
        p = f model.px1
      in
        { model | px1 = p }

    X2 f ->
      let
        p = f model.px2
      in
        { model | px2 = p }

    Y1 f ->
      let
        p = f model.py1
      in
        { model | py1 = p }

    Y2 f ->
      let
        p = f model.py1
      in
        { model | py2 = p }

type Action
  = X1 (Params -> Params)
  | X2 (Params -> Params)
  | Y1 (Params -> Params)
  | Y2 (Params -> Params)

view address model =
  div []
    [ fromElement <| collage 1000 1000
      [ trace model
      ]
    , paramControls address model
    ]

paramControls address model =
  div []
    [ Html.text "x1"
    , controlBlock (Signal.forwardTo address X1) model.px1
    , Html.text "x2"
    , controlBlock (Signal.forwardTo address X2) model.px2
    , Html.text "y1"
    , controlBlock (Signal.forwardTo address Y1) model.py1
    , Html.text "y2"
    , controlBlock (Signal.forwardTo address Y2) model.py2
    ]


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
      , max = 1.0
      , step = 0.001
      , update = \x -> Signal.message address (\p -> { p | frequency = x })
      } p.frequency
    , slider
      { title = "damping"
      , min = 0.0
      , max = 0.001
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
    [ Html.text attr.title
    , input
      [ type' "range"
      , Html.Attributes.min <| toString attr.min
      , Html.Attributes.max <| toString attr.max
      , Html.Attributes.step <| toString attr.step
      , Html.Attributes.value <| toString v
      , on "input" targetValue (parse >> attr.update)
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
    res = 3
    n = 1000
  in
    List.map (\x -> point model (x/res)) [0..(n*res)]


point : Model -> Float -> (Float, Float)
point model time =
  ( List.sum
    [ eval model.px1 time
    , eval model.px2 time
    ]
  , List.sum
    [ eval model.py1 time
    , eval model.py2 time
    ]
  )


eval : Params -> Float -> Float
eval p t =
  p.amplitude * sin (t * p.frequency + p.phase) * e ^ (-p.damping * t)
