module View.Menu exposing (render)

import View.Font as Font exposing (Text)
import View.Sprite as Sprite exposing (Sprite)
import Components.Menu as Menu exposing (Menu, MenuSection(..), HomeItem(..), MenuItem(..))
import WebGL exposing (Texture, Entity)
import View.Color as Color
import Animation exposing (Animation)
import Time
import Ease


titleAnimation : Animation
titleAnimation =
    Animation.animation 0
        |> Animation.from -24
        |> Animation.to 14
        |> Animation.ease Ease.outBounce
        |> Animation.duration (1 * Time.second)
        |> Animation.delay (0.5 * Time.second)


cursorPosition : Menu -> Float -> ( Float, Float, Float )
cursorPosition { section } yOffset =
    if section == HomeSection StartTheGame || section == MenuSection SoundOnOff then
        ( 7, 4 + yOffset, 0 )
    else
        ( 7, 15 + yOffset, 0 )


homeTop : Float
homeTop =
    40


menuTop : Float
menuTop =
    20


roundFloat : Float -> Float
roundFloat =
    round >> toFloat


render : Bool -> Texture -> Texture -> Menu -> List Entity
render sound font sprite menu =
    case menu.section of
        HomeSection _ ->
            [ Font.render Color.white newGameText font ( 15, homeTop, 0 )
            , Font.render Color.white menuText font ( 15, homeTop + 11, 0 )
            , Sprite.render selectionSprite sprite (cursorPosition menu homeTop)
            , Sprite.render mogeeSprite sprite ( 3, Animation.animate menu.time titleAnimation |> roundFloat, 0 )
            ]

        MenuSection _ ->
            [ Font.render Color.white (soundText sound) font ( 15, menuTop, 0 )
            , Font.render Color.white creditsText font ( 15, menuTop + 11, 0 )
            , Sprite.render selectionSprite sprite (cursorPosition menu menuTop)
            ]

        CreditsSection ->
            [ Font.render Color.white creditsScreenText font ( 2, 10, 0 )
            ]


selectText : Text
selectText =
    Font.text "*"


newGameText : Text
newGameText =
    Font.text "new game"


menuText : Text
menuText =
    Font.text "menu"


soundText : Bool -> Text
soundText on =
    if on then
        soundOnText
    else
        soundOffText


soundOnText : Text
soundOnText =
    Font.text "sound: on"


soundOffText : Text
soundOffText =
    Font.text "sound: off"


creditsText : Text
creditsText =
    Font.text "credits"


mogeeSprite : Sprite
mogeeSprite =
    Sprite.sprite "mogee"


creditsScreenText : Text
creditsScreenText =
    Font.text "Art, code, sound:\n@gmnabl,\n@unsoundscapes,\n@carlospazuzu."


selectionSprite : Sprite
selectionSprite =
    Sprite.sprite "life"
