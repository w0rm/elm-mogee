module View.Menu exposing (render)

import View.Font as Font exposing (Text)
import View.Sprite as Sprite exposing (Sprite)
import Components.Menu as Menu exposing (Menu(..), HomeItem(..), MenuItem(..))
import WebGL exposing (Texture, Entity)
import View.Color as Color


cursorPosition : Menu -> Float -> ( Float, Float, Float )
cursorPosition menu yOffset =
    if menu == Home StartTheGame || menu == Menu SoundOnOff then
        ( 7, 4 + yOffset, 0 )
    else
        ( 7, 15 + yOffset, 0 )


homeTop : Float
homeTop =
    40


menuTop : Float
menuTop =
    20


render : Bool -> Texture -> Texture -> Menu -> List Entity
render sound font sprite menu =
    case menu of
        Home _ ->
            [ Font.render Color.white newGameText font ( 15, homeTop, 0 )
            , Font.render Color.white menuText font ( 15, homeTop + 11, 0 )
            , Sprite.render selectionSprite sprite (cursorPosition menu homeTop)
            , Sprite.render mogeeSprite sprite ( 3, 14, 0 )
            ]

        Menu _ ->
            [ Font.render Color.white (soundText sound) font ( 15, menuTop, 0 )
            , Font.render Color.white creditsText font ( 15, menuTop + 11, 0 )
            , Sprite.render selectionSprite sprite (cursorPosition menu menuTop)
            ]

        Credits ->
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
