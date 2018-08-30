module View.Menu exposing (render)

import Animation exposing (Animation)
import Components.Menu as Menu exposing (HomeItem(..), Menu, MenuItem(..), MenuSection(..), PauseItem(..))
import Ease
import View.Color as Color
import View.Font as Font exposing (Text)
import View.Sprite as Sprite exposing (Sprite)
import WebGL exposing (Entity)
import WebGL.Texture exposing (Texture)


second : Float
second =
    1000


titleAnimation : Animation
titleAnimation =
    Animation.animation 0
        |> Animation.from -24
        |> Animation.to 14
        |> Animation.ease Ease.outBounce
        |> Animation.duration (1 * second)
        |> Animation.delay (0.5 * second)


cursorPosition : Menu -> Float -> ( Float, Float, Float )
cursorPosition { section } yOffset =
    if section == HomeSection StartTheGame || section == MenuSection SoundOnOff || section == PauseSection ResumeGame then
        ( 9, 3 + yOffset, 0 )

    else if section == HomeSection GoToMenu || section == MenuSection GoToCredits || section == PauseSection EndGame then
        ( 9, 14 + yOffset, 0 )

    else
        ( 9, 25 + yOffset, 0 )


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
            , Sprite.render logoSprite sprite ( 3, Animation.animate menu.time titleAnimation |> roundFloat, 0 )
            ]

        PauseSection _ ->
            [ Font.render Color.white resumeText font ( 15, homeTop, 0 )
            , Font.render Color.white endText font ( 15, homeTop + 11, 0 )
            , Sprite.render selectionSprite sprite (cursorPosition menu homeTop)
            ]

        MenuSection _ ->
            [ Font.render Color.white (soundText sound) font ( 15, menuTop, 0 )
            , Font.render Color.white creditsText font ( 15, menuTop + 11, 0 )
            , Font.render Color.white slidesText font ( 15, menuTop + 22, 0 )
            , Sprite.render selectionSprite sprite (cursorPosition menu menuTop)
            ]

        CreditsSection ->
            [ Font.render Color.white creditsScreenText font ( 2, 10, 0 )
            ]

        SlidesSection ->
            []


newGameText : Text
newGameText =
    Font.text "new game"


menuText : Text
menuText =
    Font.text "menu"


resumeText : Text
resumeText =
    Font.text "resume"


endText : Text
endText =
    Font.text "end game"


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


slidesText : Text
slidesText =
    Font.text "slides"


logoSprite : Sprite
logoSprite =
    Sprite.sprite "logo"


creditsScreenText : Text
creditsScreenText =
    Font.text "Art, code, sound:\n@nadyakzmn,\n@unsoundscapes,\n@carlospazuzu."


selectionSprite : Sprite
selectionSprite =
    Sprite.sprite "arrow"
