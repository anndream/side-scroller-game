module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Color exposing (Color)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Html exposing (Html, div)
import Html.Attributes as HA
import Json.Decode as JD exposing (Decoder, Value)
import Keyboard
import Keyboard.Arrows
import Task


{-| This is a simple side scrolling game made with Zinggi/elm-2d-game
-}
main : Program () Model Msg
main =
    Browser.document
        { update = update
        , init = init
        , view = view
        , subscriptions = subs
        }



-- MODEL


type alias Model =
    { time : Float
    , robin : Robin
    , running : Bool
    , camera : Camera
    , resources : Resources
    }


type alias Robin =
    { y : Float
    , vy : Float
    , x : Float
    , vx : Float
    }


initialRobin : Robin
initialRobin =
    { y = 0
    , vy = 0
    , x = 0
    , vx = 4
    }


robinsWidth : Float
robinsWidth =
    2 * (23 / 64)


robinsHeight : Float
robinsHeight =
    2 * (36 / 64)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , robin = initialRobin
      , running = False
      , camera = initialCamera
      , resources = Resources.init
      }
    , getTextures
    )


initialize : Model -> Model
initialize model =
    { model
        | time = 0
        , running = True
        , robin = initialRobin
        , camera = initialCamera
    }


gameOver : Model -> Model
gameOver model =
    { model | running = False }


initialCamera =
    Camera.fixedWidth 12.8 ( 3, 3.2 )


getTextures =
    Cmd.map Resources
        (Resources.loadTextures
            [ "images/robin-running.png"
            , "images/plx-1.png"
            , "images/plx-2.png"
            , "images/plx-3.png"
            , "images/plx-4.png"
            , "images/plx-5.png"
            ]
        )


type alias Obstacle =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


obstacles : List Obstacle
obstacles =
    [ Obstacle 10 0.0 0.5 0.8
    , Obstacle 20 0.0 1.0 1.0
    , Obstacle 30 0.4 0.6 0.8
    , Obstacle 40 0.6 0.6 0.8
    , Obstacle 50 0.8 0.6 0.8
    , Obstacle 60 1.0 0.6 0.8
    , Obstacle 70 1.2 0.6 0.8
    , Obstacle 80 1.4 0.6 0.8
    , Obstacle 90 1.6 0.6 0.8
    , Obstacle 94 1.8 0.6 0.8
    , Obstacle 96 2.0 0.6 0.8
    ]



-- UPDATE


type Msg
    = MouseDown MousePosition
    | Tick Float
    | Resources Resources.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        MouseDown _ ->
            if model.running then
                { model
                    | robin = jump model.robin
                }

            else
                initialize model

        Tick dt ->
            if model.running then
                if thereIsCollision model.robin then
                    gameOver model

                else
                    { model
                        | time = model.time + dt
                        , robin = tick dt model.robin
                        , camera =
                            Camera.moveBy ( dt * model.robin.vx, 0 ) model.camera
                    }

            else
                model

        Resources rMsg ->
            { model | resources = Resources.update rMsg model.resources }


jump : Robin -> Robin
jump guy =
    if guy.y == 0 then
        { guy | vy = 7.0 }

    else
        guy


tick : Float -> Robin -> Robin
tick dt guy =
    guy
        |> gravity dt
        |> physics dt


gravity : Float -> Robin -> Robin
gravity dt guy =
    { guy
        | vy =
            if guy.y >= 0 then
                guy.vy - 9.81 * dt

            else
                0
    }


physics : Float -> Robin -> Robin
physics dt guy =
    { guy
        | x = guy.x + dt * guy.vx
        , y = max 0 (guy.y + dt * guy.vy)
    }


thereIsCollision : Robin -> Bool
thereIsCollision robin =
    let
        allTrue =
            List.all identity

        collides obs =
            -- the convention is
            --     x == left
            --     y == bottom
            --     x + width == right
            --     y + height == top
            allTrue
                [ -- robin's left is on the left side of obstacle's right
                  robin.x < obs.x + obs.width
                , -- robin's bottom is lower than obstacle's top
                  robin.y < obs.y + obs.height
                , -- robin's right is on the right side of obstacle's left
                  robin.x + robinsWidth > obs.x
                , -- robin's top is higher than obstacle's bottom
                  robin.y + robinsHeight > obs.y
                ]
    in
    List.any collides obstacles



-- SUBSCRIPTIONS


type alias MousePosition =
    { x : Int, y : Int }


mousePosition : Decoder MousePosition
mousePosition =
    JD.map2 MousePosition
        (JD.field "clientX" JD.int)
        (JD.field "clientY" JD.int)


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
        , Browser.Events.onMouseDown (JD.map MouseDown mousePosition)
        ]



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Side Scrolling Game"
    , body =
        [ Game.renderCentered
            { camera = model.camera
            , time = model.time
            , size = ( 1024, 512 )
            }
            (render model)
        ]
    }


render : Model -> List Renderable
render model =
    renderBackground model.resources
        ++ List.map renderObstacle obstacles
        ++ [ renderRobin model.resources model.robin ]


renderObstacle : Obstacle -> Renderable
renderObstacle obs =
    Render.shape Render.rectangle
        { color = Color.red
        , position = ( obs.x, obs.y )
        , size = ( obs.width, obs.height )
        }


renderRobin : Resources -> Robin -> Renderable
renderRobin resources { x, y } =
    Render.animatedSpriteWithOptions
        { position = ( x, y, 0 )
        , size = ( robinsWidth, robinsHeight )
        , texture = Resources.getTexture "images/robin-running.png" resources
        , bottomLeft = ( 0, -36 / 64 )
        , topRight = ( 23 / 64, 0 )
        , duration = 0.6
        , numberOfFrames = 8
        , rotation = 0
        , pivot = ( 0, 0 )
        }


renderBackground : Resources -> List Renderable
renderBackground resources =
    let
        layer z path ss =
            Render.parallaxScrollWithOptions
                { z = z
                , texture = Resources.getTexture path resources
                , scrollSpeed = ( ss, ss )
                , tileWH = ( 1, 2 )
                , offset = ( 0, 0 )
                }
    in
    [ layer -0.99 "images/plx-1.png" 0
    , layer -0.98 "images/plx-2.png" 0.2
    , layer -0.97 "images/plx-3.png" 0.5
    , layer -0.96 "images/plx-4.png" 0.8
    , layer -0.95 "images/plx-5.png" 1.35
    ]
