module Main exposing (..)

import Html exposing (Html, div, text, program)
import Html.Events exposing (onClick)
import Html.Attributes as HtmlAttr

import Svg exposing (Svg, svg, circle) 
import Svg.Attributes exposing (..)

import Math.Vector2 as V2

import Mouse
import Window
import Debug

import Basics exposing (..)


-- MODEL

type alias Screen = {
        w : Int,
        h : Int
    }

type alias Model = {
        screen : Screen,
        current_pos : V2.Vec2,
        goto_pos : V2.Vec2
    }


init : ( Model, Cmd Msg )
init =
    ( { 
        screen = { w = 1024, h = 1024 },
        current_pos = V2.vec2 100 100,
        goto_pos =  V2.vec2 100 100 
      }
      , Cmd.none )

update_screen : Model -> (Int, Int) -> Model
update_screen model pair =
    let 
      (a, b)  = Debug.log "resize" pair
    in
      { model | screen = { w = a, h = b} }

update_pos : Model -> Mouse.Position -> Model
update_pos model msg  = 
     let
         x = toFloat msg.x
         y = toFloat msg.y
     in
      { model | goto_pos = V2.vec2 x y}

-- MESSAGES

type Msg
    = MouseMsg Mouse.Position
    | Resize Int Int
    | MsgNone


-- VIEW

view2 : Model -> Svg msg
view2 model =
        let 
             x = (toString (V2.getX model.goto_pos))
             y = (toString (V2.getY model.goto_pos))
        --     log1 = Debug.log "xvalue: " x 
        --     log2 = Debug.log "yvalue: " y 
        in 
             circle [ cx x, cy y, r "15", fill  "rgba(0, 128, 255, 1)" ] []

view : Model -> Html msg
view model =
    let 
        wi = toString model.screen.w
        he = toString model.screen.h
    in
        Html.div [
             HtmlAttr.style [ ("backgroundColor", "#778899") , ( "height", "100vh") ] 
           ]
           [    
               svg [ width wi, height he ]
               [
                    view2 model 
               ]
           ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg position ->
            ( update_pos model position, Cmd.none )
        Resize w h -> 
            ( update_screen  model (w, h), Cmd.none )
        MsgNone ->
            (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ 
            Mouse.clicks MouseMsg,
            Window.resizes (\size -> Resize size.width size.height)
        ]


-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
