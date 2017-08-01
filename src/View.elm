module View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (autoplay, height, loop, src, style, width)
import Messages exposing (Msg)
import Model exposing (GameState(Playing), Model)
import Model.Object exposing (invertScreen, isScreen)
import View.Common as Common
import View.Color as Color
import View.Lives as Lives
import View.Object as Object
import WebGL exposing (Entity, Texture)


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
            Model.mogee model |> .position

        offset =
            ( x - 32 + 4, y - 32 + 5 )

        ( cx, cy ) =
            toMinimap ( x, y )

        allScr =
            model.objects
                |> List.filter isScreen
                |> List.map (.position >> toMinimap)

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

        monster { position, size } =
            Common.rectangle size ( Tuple.first position - Tuple.first offset, Tuple.second position - Tuple.second offset, 2 ) Color.darkBlue

        offsetObject ({ position } as object) =
            { object
                | position = ( Tuple.first position - Tuple.first offset, Tuple.second position - Tuple.second offset )
            }
    in
        if model.state == Model.Stopped then
            (if model.score > 0 then
                (Lives.renderScore texture ( 32, 1, 0 ) model.score)
             else
                []
            )
                ++ [ Lives.renderTitle texture ( 3, 14 )
                   , Lives.renderPlay font ( 11, 40, 0 )
                   ]
        else
            (if model.state == Model.Paused then
                [ Lives.renderPlay font ( 11, 40, 0 ) ]
             else
                []
            )
                ++ Lives.renderLives texture ( 1, 1, 0 ) model.lives
                ++ Lives.renderScore texture ( 32, 1, 0 ) (model.currentScore + model.score)
                ++ List.map dot allScr
                ++ List.foldl (Object.render texture) [] (List.map offsetObject model.objects)
