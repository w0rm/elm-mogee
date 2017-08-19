module View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (autoplay, height, loop, src, style, width)
import Messages exposing (Msg)
import Components.Keys as Keys
import Model exposing (GameState(Playing), Model)
import View.Common as Common
import View.Color as Color
import View.Lives as Lives
import View.Components
import Components.Components as Components
import WebGL exposing (Entity, Texture)
import View.Font as Font exposing (Text)


withSound : GameState -> List (Html Msg) -> List (Html Msg)
withSound state =
    case state of
        Playing ->
            (::) (Html.audio [ src "../snd/theme.ogg", autoplay True, loop True ] [])

        _ ->
            identity


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", "0" )
            , ( "top", "0" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background", "#000" )
            ]
        ]
        (withSound model.state
            [ WebGL.toHtmlWith
                [ WebGL.depth 1
                , WebGL.clearColor (22 / 255) (17 / 255) (22 / 255) 0
                ]
                [ width model.size
                , height model.size
                , style
                    [ ( "display", "block" )
                    , ( "position", "absolute" )
                    , ( "top", "50%" )
                    , ( "left", "50%" )
                    , ( "margin-top", toString (-model.size // 2) ++ "px" )
                    , ( "margin-left", toString (-model.size // 2) ++ "px" )
                    , ( "image-rendering", "optimizeSpeed" )
                    , ( "image-rendering", "-moz-crisp-edges" )
                    , ( "image-rendering", "-webkit-optimize-contrast" )
                    , ( "image-rendering", "crisp-edges" )
                    , ( "image-rendering", "pixelated" )
                    , ( "-ms-interpolation-mode", "nearest-neighbor" )
                    ]
                ]
                (Maybe.map2 (render model) model.texture model.font
                    |> Maybe.withDefault []
                )
            ]
        )


toMinimap : ( Float, Float ) -> ( Float, Float )
toMinimap ( x, y ) =
    ( floor (x / 64) |> toFloat
    , floor (y / 64) |> toFloat
    )


render : Model -> Texture -> Texture -> List Entity
render model texture font =
    let
        ( x, y ) =
            Components.mogeeOffset model.components

        offset =
            ( toFloat (round x - 28), toFloat (round y - 27) )

        ( cx, cy ) =
            toMinimap ( x, y )

        allScr =
            Components.foldl2
                (\_ _ { position } positions -> toMinimap position :: positions)
                []
                model.components.screens
                model.components.transforms

        maxX =
            List.maximum (List.map Tuple.first allScr) |> Maybe.withDefault 0

        minY =
            List.minimum (List.map Tuple.second allScr) |> Maybe.withDefault 0

        dot ( x1, y1 ) =
            Common.rectangle
                ( 1, 1 )
                ( 63 - maxX - 1 + x1, y1 - minY + 1, 0 )
                (if x1 == cx && y1 == cy then
                    Color.yellow
                 else
                    Color.gray
                )
    in
        if model.state == Model.Stopped then
            (if model.score > 0 then
                (Lives.renderScore texture ( 32, 1, 0 ) model.score)
             else
                []
            )
                ++ [ Lives.renderTitle texture ( 3, 14 )
                   , renderPlay font
                   ]
        else
            (if model.state == Model.Paused then
                [ renderPlay font ]
             else
                []
            )
                ++ Lives.renderLives texture ( 1, 1, 0 ) model.lives
                ++ Lives.renderScore texture ( 32, 1, 0 ) (model.systems.currentScore + model.score)
                ++ List.map dot allScr
                ++ View.Components.render texture (Keys.directions model.keys).x offset model.components []


playText : Text
playText =
    Font.text "press enter\n   to play"


renderPlay : Texture -> Entity
renderPlay texture =
    Font.render Color.white playText texture ( 12, 40, 0 )
