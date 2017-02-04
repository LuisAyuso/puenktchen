module Main exposing (..)

import Html exposing (Html, div, text, program)
import Html.Events exposing (onClick)
import Html.Attributes as HtmlAttr

import Svg exposing (Svg, svg, circle) 
import Svg.Attributes exposing (..)

import Math.Vector2 as V2

-- move this to the parent, turn this file into a module
import Mouse
import Window
import Debug
import AnimationFrame

import Basics exposing (..)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- MODEL
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type alias Screen = {
        w : Int,
        h : Int
    }

type alias Model = {
        screen : Screen,
        current_pos : V2.Vec2,
        goto_pos : V2.Vec2,
        speed : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { 
        screen = { w = 1024, h = 1024 },
        current_pos = V2.vec2 100 100,
        goto_pos =  V2.vec2 100 100 ,
        speed = 1.0
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

update_model : Model -> Float -> Model
update_model model dt =
    let 
        dir = V2.direction model.current_pos model.goto_pos
        _ = Debug.log "dir: " dir
        delta = V2.scale dt dir
        _ = Debug.log "delta: " dir
    in
        { model | current_pos = V2.add model.current_pos delta}

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- MESSAGES
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Msg
    = MouseMsg Mouse.Position
    | Resize Int Int
    | Update Float
    | MsgNone


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- VIEW
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

view2 : Model -> Svg msg
view2 model =
        let 
             x = (toString (V2.getX model.current_pos))
             y = (toString (V2.getY model.current_pos))
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

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- UPDATE
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg position ->
            (update_pos model position, Cmd.none)
        Resize w h -> 
            (update_screen  model (w, h), Cmd.none)
        Update f -> 
            (update_model model f, Cmd.none)
        MsgNone ->
            (model, Cmd.none)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- SUBSCRIPTIONS
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ 
            Mouse.clicks MouseMsg,
            Window.resizes (\size -> Resize size.width size.height),
            AnimationFrame.diffs Update
        ]

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- MAIN
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
