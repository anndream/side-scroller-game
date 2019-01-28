module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Color exposing (Color)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Json.Decode as JD exposing (Decoder, Value)


{-| A simple side scrolling game made with Zinggi/elm-2d-game
-}
main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }



-- MODEL


type alias Model =
    { time : Float
    , robin : Robin
    , camera : Camera
    , resources : Resources
    }


type alias Robin =
    { x : Float
    , vx : Float
    , y : Float
    , vy : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , robin = { x = 0, vx = 0, y = 0, vy = 0 }
      , camera = initialCamera
      , resources = Resources.init
      }
    , Cmd.map GotResources
        (Resources.loadTextures
            [ "images/robin-running.png"
            , "images/plx-1.png"
            , "images/plx-2.png"
            , "images/plx-3.png"
            , "images/plx-4.png"
            , "images/plx-5.png"
            ]
        )
    )


initialCamera : Camera
initialCamera =
    Camera.fixedWidth 14 ( 3, 3.5 )


robinsSize : ( Float, Float )
robinsSize =
    ( 23 / 30, 36 / 30 )


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
    | GotResources Resources.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        MouseDown _ ->
            if isRunning model.robin then
                { model | robin = jumpIfBelowZero model.robin }

            else
                { model
                    | time = 0
                    , robin = { x = 0, vx = 4, y = 0, vy = 0 }
                    , camera = initialCamera
                }

        Tick dt ->
            if isRunning model.robin then
                if collidesWithAnObstacle model.robin then
                    { model | robin = freeze model.robin }

                else
                    { model
                        | time = model.time + dt
                        , robin = tick dt model.robin
                        , camera =
                            model.camera
                                |> Camera.moveBy ( dt * model.robin.vx, 0 )
                    }

            else
                model

        GotResources rMsg ->
            { model | resources = model.resources |> Resources.update rMsg }


isRunning : Robin -> Bool
isRunning r =
    r.vx > 0


run : Robin -> Robin
run r =
    { r | vx = 4 }


jumpIfBelowZero : Robin -> Robin
jumpIfBelowZero r =
    if r.y > 0 then
        r

    else
        { r | vy = 7.0 }


freeze : Robin -> Robin
freeze r =
    { r | vx = 0, vy = 0 }



--


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



--


collidesWithAnObstacle : Robin -> Bool
collidesWithAnObstacle robin =
    let
        ( robinsWidth, robinsHeight ) =
            robinsSize

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
        [ Browser.Events.onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
        , Browser.Events.onMouseDown (JD.map MouseDown mousePosition)
        ]



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Side Scrolling Game"
    , body =
        [ Game.TwoD.renderCentered
            { camera = model.camera
            , time = model.time
            , size = ( 1200, 600 )
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
        , size = robinsSize
        , texture = Resources.getTexture "images/robin-running.png" resources
        , bottomLeft = ( 0, -36 / 64 )
        , topRight = ( 23 / 64, 0 )
        , duration = 0.5
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
                , tileWH = ( 1.17, 2.34 )
                , offset = ( 0, 0.8 )
                }
    in
    [ layer -0.99 "images/plx-1.png" 0
    , layer -0.98 "images/plx-2.png" 0.2
    , layer -0.97 "images/plx-3.png" 0.5
    , layer -0.96 "images/plx-4.png" 0.7
    , layer -0.95 "images/plx-5.png" 0.9
    ]
