module Battery exposing (..)

import BatteryStatus exposing (..)
import Task exposing (Task)
import Html.App as App
import Svg.Attributes as SA exposing (..)
import Svg exposing (..)
import Html.App as App
import String
import Color exposing (Color)


-- TYPES


type alias Stroke =
    { stroke : String
    , width : Float
    }


type alias BatteryView =
    { h : Float
    , w : Float
    , thickness : Float
    , spacing : Float
    , bars : Int
    , chargeColor : Color
    , defaultColor : Color
    }


type ViewType
    = Supported BatteryStatus
    | Unsupported String


type alias Model =
    { view : BatteryView
    , viewType : ViewType
    }


type Msg
    = NoSupport
    | BatteryStatusChange BatteryStatus



-- SAMPLES


sample1 : BatteryView
sample1 =
    { h = 480
    , w = 300
    , thickness = 24
    , spacing = 20
    , bars = 4
    , chargeColor = Color.red
    , defaultColor = Color.black
    }



-- PROGRAM


main : Program Never
main =
    App.program
        { init = init sample1
        , update = update
        , view = view
        , subscriptions = (\_ -> BatteryStatus.changes BatteryStatusChange)
        }


init : BatteryView -> ( Model, Cmd Msg )
init view =
    ( { view = view
      , viewType = Unsupported "Battery status is not supported"
      }
    , Task.perform (\_ -> NoSupport) BatteryStatusChange BatteryStatus.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoSupport ->
            model ! []

        BatteryStatusChange batteryStatus ->
            { model | viewType = Supported batteryStatus } ! []



-- VIEW


view : Model -> Svg Msg
view model =
    let
        { w, h } =
            model.view

        content =
            case model.viewType of
                Supported status ->
                    Svg.g []
                        [ Svg.g
                            [ SA.transform ("translate(0," ++ toString h ++ ") scale(1,-1)")
                            ]
                            [ shell model
                            , level model
                            ]
                        , time model
                        ]

                Unsupported string ->
                    Svg.text'
                        [ x "0"
                        , y "0"
                        , fontFamily "Arial Black"
                        , fontSize "45"
                        ]
                        [ text string ]
    in
        Svg.svg
            [ SA.version "1.1"
            , SA.width <| toString w
            , SA.height <| toString h
            , SA.viewBox <| viewBoxCenter w h
            ]
        <|
            [ defs model.view ]
                ++ [ content
                   , Svg.circle [ cx "0", cy "0", r "3", fill "red" ] []
                   ]


defs : BatteryView -> Svg Msg
defs view =
    Svg.defs []
        [ Svg.filter [ id "filter", x "0", y "0" ]
            [ Svg.feGaussianBlur [ SA.stdDeviation "5" ] []
            , Svg.feOffset [ dx "15", dy "15" ] []
            ]
        , linearGradient
            [ id "charging"
            , x1 "100%"
            , y1 "100%"
            ]
            [ myStop [ stopAnimation view ]
            , myStop [ stopAnimation view, offsetAnimation ]
            ]
        , gradient "full" "#b2ed9d" "#569b3d"
        , gradient "half" "orange" "lightred"
        , gradient "empty" "lightred" "red"
        ]


shell : Model -> Svg Msg
shell { view, viewType } =
    case viewType of
        Supported status ->
            let
                { w, h, thickness, defaultColor } =
                    view

                gradStroke =
                    { stroke = "url(#" ++ (grad status True) ++ ")", width = thickness }
            in
                Svg.g []
                    [ -- svgRect 0 0 w h { stroke = (color' defaultColor), width = 1 } "none"
                      frame view gradStroke
                    , terminal view gradStroke
                    ]

        _ ->
            empty


frame : BatteryView -> Stroke -> Svg a
frame ({ w, thickness } as view) gradStroke =
    let
        t2sp =
            thickness / 2
    in
        svgRect t2sp
            t2sp
            (w - thickness)
            (frameHeight view)
            gradStroke
            "none"


frameHeight : BatteryView -> Float
frameHeight ({ h, spacing, thickness } as vm) =
    0.9 * (h - thickness - (2 * spacing))


terminal : BatteryView -> Stroke -> Svg a
terminal ({ w, h, thickness } as vm) gradStroke =
    let
        term =
            terminalHeight vm
    in
        svgRect (0.35 * w) (h - term - thickness / 2) (0.3 * w) term gradStroke "none"


terminalHeight : BatteryView -> Float
terminalHeight { h, thickness, spacing } =
    0.1 * (h - thickness - (2 * spacing))


powerHeight : BatteryView -> Float
powerHeight ({ spacing, thickness } as vm) =
    (frameHeight vm) - 2 * spacing - thickness


level : Model -> Svg Msg
level { view, viewType } =
    case viewType of
        Supported status ->
            Svg.g []
                (fst <|
                    List.foldl
                        (barf status view)
                        ( [], status.level )
                        [0..(view.bars - 1)]
                )

        _ ->
            empty


barf : BatteryStatus -> BatteryView -> Int -> ( List (Svg Msg), Float ) -> ( List (Svg Msg), Float )
barf status ({ bars, thickness, spacing, w } as vm) idx ( list, power ) =
    if power <= 0 then
        ( list, 0 )
    else
        let
            drainPerBar =
                1 / (toFloat bars)

            barPercent =
                Basics.min 1 (power / drainPerBar)

            barWidth =
                w - 2 * (spacing + thickness)

            barOffset =
                offY vm <| toFloat idx
        in
            ( list
                ++ [ svgRect (thickness + spacing)
                        barOffset
                        barWidth
                        ((barHeight vm) * barPercent)
                        { stroke = "none", width = 0 }
                        ("url(#" ++ (grad status False) ++ ")")
                   ]
            , power - drainPerBar
            )


time : Model -> Svg Msg
time { view, viewType } =
    case viewType of
        Supported status ->
          Svg.g [] [ showTime (view.thickness) ((view.thickness + 2 * view.spacing)  + terminalHeight view) (view.h  / 8) status.dischargingTime 90
                   , showTime (view.w - view.thickness) (view.h - view.thickness) (view.h  / 8) status.chargingTime -90
                   ]
        _ ->
            empty


showTime : a -> b -> c -> d -> e ->  Svg f
showTime cx cy size text rot =
    Svg.text'
        [ x "0"
        , y "0"
        , transform <| "translate(" ++ (toString cx) ++ ", " ++ (toString cy) ++ ") rotate (" ++ (toString rot) ++ " 0 0)"
        , fontFamily "Arial Black"
        , fontSize <| toString size
        ]
        [ Svg.text <| Debug.log "cht" <| toString text ]


-- CALC HELPERS


offY : BatteryView -> Float -> Float
offY ({ spacing, thickness } as vm) idx =
    idx * ((barHeight vm) + vm.spacing) + vm.thickness + vm.spacing


barHeight : BatteryView -> Float
barHeight vm =
    let
        barF =
            toFloat vm.bars
    in
        (powerHeight vm - ((barF - 1) * vm.spacing)) / barF


grad : BatteryStatus -> Bool -> String
grad status withCharging =
    if status.isCharging && withCharging then
        "charging"
    else if status.level > 0.5 then
        "full"
    else if (status.level > 0.25 && status.level < 0.5) then
        "half"
    else
        "empty"



-- gradients


myStop : List (Svg a) -> Svg a
myStop anims =
    stop
        [ offset "0%"
        , stopColor "lightblue"
        , stopOpacity ".5"
        ]
        anims


stopAnimation : BatteryView -> Svg a
stopAnimation { chargeColor, defaultColor } =
    animate
        [ attributeName "stop-color"
        , values <| String.join ";" [ color' chargeColor, color' defaultColor, color' chargeColor ]
        , dur "3s"
        , repeatCount "indefinite"
        ]
        []


offsetAnimation : Svg a
offsetAnimation =
    animate
        [ attributeName "offset"
        , values ".75;.50;.25;.125;0;.125;.25;.50;.75"
        , dur "1s"
        , repeatCount "indefinite"
        ]
        []



-- UTILS


empty : Svg a
empty =
    Svg.g [] []


viewBoxCenter : Float -> Float -> String
viewBoxCenter w h =
    String.join " " <| List.map toString [ -w / 2, -h / 2, 2 * w, 2 * h ]


svgRect : Float -> Float -> Float -> Float -> Stroke -> String -> Svg e
svgRect fx fy fw fh stroke fill =
    Svg.rect
        [ x <| toString fx
        , y <| toString fy
        , SA.width <| toString fw
        , height <| toString fh
        , SA.stroke stroke.stroke
        , SA.strokeWidth <| toString stroke.width
        , SA.fill fill
        , strokeLinecap "butt"
        ]
        []


gradient : String -> String -> String -> Svg a
gradient myId col0 col100 =
    linearGradient [ id myId ]
        [ stop [ offset "0", stopColor col0 ] []
        , stop [ offset "100%", stopColor col100 ] []
        ]


color' : Color -> String
color' c =
    let
        { red, green, blue, alpha } =
            Color.toRgb c
    in
        "rgba(" ++ (String.join "," <| List.map toString [ red, green, blue ]) ++ ", 1)"
