module View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (height, style, width)
import Messages exposing (Msg)
import Components.Keys as Keys
import Model exposing (GameState(..), Model)
import View.Common as Common
import View.Color as Color
import View.Lives as Lives
import View.Components
import Components.Components as Components
import WebGL exposing (Entity, Texture)
import View.Font as Font exposing (Text)
import View.Menu as Menu
import Slides.View as Slides
import Components.Menu as Menu


view : Model -> Html Msg
view model =
    WebGL.toHtmlWith
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
        (Maybe.map3 (render model)
            model.texture
            model.font
            model.sprite
            |> Maybe.withDefault []
        )


toMinimap : ( Float, Float ) -> ( Float, Float )
toMinimap ( x, y ) =
    ( floor (x / 64) |> toFloat
    , floor (y / 64) |> toFloat
    )


render : Model -> Texture -> Texture -> Texture -> List Entity
render model texture font sprite =
    case model.state of
        Initial menu ->
            if (menu.section == Menu.SlidesSection) then
                Slides.render sprite font model.slides
            else
                Menu.render model.sound font sprite menu

        Paused menu ->
            Menu.render model.sound font sprite menu
                ++ renderGame model texture font sprite

        Dead ->
            Font.render Color.white continueText font ( 12, 40, 0 )
                :: renderGame model texture font sprite

        Playing ->
            renderGame model texture font sprite


renderGame : Model -> Texture -> Texture -> Texture -> List Entity
renderGame model texture font sprite =
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
        Lives.renderLives sprite ( 1, 1, 0 ) model.lives
            ++ Lives.renderScore texture ( 32, 1, 0 ) (model.systems.currentScore + model.score)
            ++ List.map dot allScr
            ++ View.Components.render texture (Keys.directions model.keys).x offset model.components []


continueText : Text
continueText =
    Font.text "press enter\nto continue"
