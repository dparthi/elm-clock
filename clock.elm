import Html exposing (Html, div, button, input, br)
import Html.Events exposing (onClick, onInput, onDoubleClick)
import Html.Attributes exposing (value)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Date exposing (Date)
import Debug exposing (log)
import String exposing (reverse, cons)

main =
 Html.program
   { init = init
   , view = view
   , update = update
   , subscriptions = subscriptions
   }

-- MODEL
type alias Clock = {id: Int, t: Time, col: String, label: String, x : Float, y : Float}

--type alias Clocks = List Clock

type alias Model = List Clock

defaultClock : Clock
defaultClock = {id = 0, t = 0, col="ffffff", label="London", x = 50, y = 50}

clock1 : Clock
clock1 = {id = 1, t = 0, col="abcdef", label="Bangalore", x = 100, y = 50}

-- clocks = [clock1, defaultClock]

init : (Model, Cmd Msg)
init =
 ([clock1, defaultClock], Cmd.none)

-- UPDATE
type Msg
 = Tick Time
 | Input String String
 | Reset

updateClocks : Time -> Model -> Model
updateClocks time model =
  List.map (\c -> {c | t = time}) model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
 case msg of
   Tick newTime ->
     ( updateClocks newTime model , Cmd.none)
   Reset ->
     ( [clock1, defaultClock], Cmd.none)
   Input id str ->
     ( List.map (\c -> if ((toString c.id) == id) then ({c | col = str}) else c) model, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
 Time.every second Tick

-- VIEW
view : Model -> Html Msg
view model =
  div [] ((List.map drawClock model) ++ [button [ onClick Reset ] [ text "RESET" ]])

drawClock : Clock -> Html Msg
drawClock clk =
  let
   clockId = toString clk.id
   centerX = clk.x
   centerY = clk.y
   circleRadius = 45
   secHandRadius = 40
   minHandRadius = 30
   hourHandRadius = 18
   epochSecs = Time.inSeconds clk.t + (5 * 3600 + 30 * 60) -- GMT+5:30
   hours = floor (toFloat (floor(epochSecs) % (24 * 3600)) / 3600)
   mins = floor (toFloat (floor(epochSecs) % 3600) / 60)
   seconds = (floor epochSecs) % 60
   radians mins = (toFloat mins) * (22/(7*30))
 in
 div [] [
   svg [ viewBox "0, 0, 300,300" , width "200px" ]
     [ clockCircle centerX centerY circleRadius (String.cons '#' clk.col)
     , clockHand centerX centerY secHandRadius (radians seconds)
     , clockHand centerX centerY minHandRadius (radians mins)
     , clockHand centerX centerY hourHandRadius (radians (5 * hours))
     ]
   , text clk.label
   , br [] []
   , input [onInput (Input clockId), value clk.col] []
  ]

clockCircle centerX centerY radius col =
 circle [ cx (toString centerX),
          cy (toString centerY),
          r (toString radius), fill col ] []

clockHand centerX centerY armRadius radians =
 let
   angle = radians - 1.5714
   handX = toString (centerX + armRadius * cos angle)
   handY = toString (centerY + armRadius * sin angle)
 in
   log (toString angle)
   line [ x1 (toString centerX), y1 (toString centerY),
              x2 handX, y2 handY, stroke "#023963" ] []
