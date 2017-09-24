module View.Font
    exposing
        ( text
        , render
        , Text
        , load
        , name
        )

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture exposing (Error, defaultOptions)
import Dict exposing (Dict)
import String
import Task
import View.FontData exposing (font, fontSrc, CharInfo)


emHeight : Float
emHeight =
    11


spaceWidth : Float
spaceWidth =
    3


tracking : Float
tracking =
    1


type Text
    = Text String (Mesh Vertex)


name : Text -> String
name (Text name _) =
    name


invertDict : List ( Int, List String ) -> Dict String Int
invertDict =
    List.foldl
        (\( n, chars ) dict -> List.foldl (\char -> Dict.insert char n) dict chars)
        Dict.empty


{-| Defines the left kerning classes
-}
leftKerningClass : Dict String Int
leftKerningClass =
    invertDict
        [ ( 1, [ "A", "B", "C", "D", "E", "G", "H", "I", "J", "K", "M", "N", "O", "Q", "R", "S", "U", "V", "W", "X", "Z", "l" ] )
        , ( 2, [ "F", "P" ] )
        , ( 3, [ "L" ] )
        , ( 4, [ "T" ] )
        , ( 5, [ "b", "k", "p", "s", "t", "u", "v", "w", "x", "z", "e" ] )
        , ( 6, [ "f", "ff" ] )
        , ( 7, [ "g", "q", "y" ] )
        , ( 8, [ "i", "fi", "ffi" ] )
        , ( 9, [ "j", "fj", "jj" ] ) --?
        , ( 10, [ ".", "," ] )
        , ( 11, [ "'", "\"" ] )
        , ( 12, [ "!", "?" ] )
        , ( 13, [ "/" ] )
        , ( 14, [ "Y", "7" ] )
        , ( 15, [ "(", "[" ] )
        , ( 16, [ "a", "c", "h", "m", "n", "o", "r" ] ) --5
        , ( 17, [ "1", "2", "3", "4", "5", "6", "8", "9" ] ) --5
        ]


{-| Defines the right kerning classes
-}
rightKerningClass : Dict String Int
rightKerningClass =
    invertDict
        [ ( 1, [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "U", "V", "W", "X", "Z" ] )
        , ( 2, [ "J" ] )
        , ( 3, [ "T", "Y" ] )
        , ( 4, [ "a", "c", "m", "n", "o", "q", "r", "t", "u", "v", "w", "x", "y", "z" ] )
        , ( 5, [ "b", "h", "i", "k", "l" ] )
        , ( 6, [ "e", "d", "g", "p", "s", "ss" ] )
        , ( 7, [ "f", "ff", "fi", "ffi", "fj" ] )
        , ( 8, [ "j", "jj" ] )
        , ( 9, [ "7" ] )
        , ( 10, [ "." ] )
        , ( 11, [ "'", "\"" ] )
        , ( 12, [ "!", "?", ":" ] )
        , ( 13, [ "/" ] )
        , ( 14, [ ",", ";" ] )
        , ( 15, [ "$" ] )
        ]


{-| Defines the kerning between the left and right kerning classes
-}
kerningDict : Dict ( Int, Int ) Float
kerningDict =
    Dict.fromList
        [ ( ( 1, 14 ), -1 )
        , ( ( 2, 2 ), -1 )
        , ( ( 2, 6 ), 0 )
        , ( ( 2, 14 ), -1 )
        , ( ( 14, 2 ), -1 )
        , ( ( 14, 6 ), -1 )
        , ( ( 2, 10 ), -1 )
        , ( ( 2, 13 ), -1 )
        , ( ( 3, 3 ), -1 )
        , ( ( 3, 7 ), -1 )
        , ( ( 3, 14 ), -1 )
        , ( ( 4, 2 ), -2 )
        , ( ( 4, 4 ), -1 )
        , ( ( 4, 6 ), -1 )
        , ( ( 4, 7 ), -1 )
        , ( ( 4, 10 ), -1 )
        , ( ( 4, 13 ), -2 )
        , ( ( 4, 14 ), -2 )
        , ( ( 5, 3 ), -1 )
        , ( ( 5, 14 ), -1 )
        , ( ( 16, 3 ), -1 )
        , ( ( 16, 7 ), -1 )
        , ( ( 16, 9 ), -1 )
        , ( ( 16, 14 ), -1 )
        , ( ( 6, 2 ), -2 )
        , ( ( 6, 3 ), -1 )
        , ( ( 6, 4 ), -1 )
        , ( ( 6, 5 ), -1 )
        , ( ( 6, 6 ), -1 )
        , ( ( 6, 7 ), -2 )
        , ( ( 6, 13 ), -2 )
        , ( ( 6, 14 ), -2 )
        , ( ( 7, 3 ), -1 )
        , ( ( 7, 7 ), 0 )
        , ( ( 8, 7 ), 0 )
        , ( ( 13, 2 ), -1 )
        , ( ( 13, 14 ), -1 )
        , ( ( 14, 10 ), -1 )
        , ( ( 14, 14 ), -1 )
        , ( ( 15, 2 ), -1 )
        , ( ( 15, 13 ), -1 )
        , ( ( 17, 14 ), -1 )
        ]


{-| Defines the custom kerning overrides
-}
kerningOverrides : Dict ( String, String ) Float
kerningOverrides =
    Dict.fromList
        [ ( ( "/", "/" ), -2 ) -- example
        , ( ( "\\", "\\" ), -2 )
        , ( ( "C", "f" ), -1 )
        , ( ( "I", "f" ), -1 )
        , ( ( "f", "T" ), -2 )
        , ( ( "q", "f" ), -1 )
        , ( ( "q", "fi" ), -1 )
        , ( ( "q", "ffi" ), -1 )
        , ( ( "q", "ff" ), -1 )
        ]


{-| Side bearings, left and right
-}
bearingsDict : Dict String ( Float, Float )
bearingsDict =
    Dict.fromList
        [ ( "j", ( -2, 1 ) )
        , ( "jj", ( -2, 1 ) )
        , ( ",", ( 0, 1 ) )
        , ( ";", ( 0, 1 ) )
        , ( " ", ( 0, 0 ) )

        -- , ( "^", ( 0, 0 ) )
        ]


defaultBearings : ( Float, Float )
defaultBearings =
    ( 0, 1 )


rightBearing : String -> Float
rightBearing char =
    Dict.get char bearingsDict
        |> Maybe.withDefault defaultBearings
        |> Tuple.second


leftBearing : String -> Float
leftBearing char =
    Dict.get char bearingsDict
        |> Maybe.withDefault defaultBearings
        |> Tuple.first


kerning : String -> String -> Float
kerning prevChar nextChar =
    List.filterMap identity
        [ Dict.get ( prevChar, nextChar ) kerningOverrides
        , Maybe.map2
            (curry ((flip Dict.get) kerningDict))
            (Dict.get prevChar leftKerningClass)
            (Dict.get nextChar rightKerningClass)
            |> Maybe.andThen identity
        ]
        |> List.head
        |> Maybe.withDefault 0


type alias Vertex =
    { position : Vec2
    , texPosition : Vec2
    }


load : (Result Error Texture -> msg) -> Cmd msg
load msg =
    Texture.loadWith
        { defaultOptions
            | magnify = Texture.nearest
            , minify = Texture.nearest
            , flipY = False
        }
        fontSrc
        |> Task.attempt msg


render : Vec3 -> Text -> Texture -> ( Float, Float, Float ) -> Entity
render color (Text _ mesh) texture offset =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        mesh
        { offset = Vec3.fromTuple offset
        , texture = texture
        , color = color
        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
        }


text : String -> Text
text text =
    let
        chars =
            List.reverse (replaceLigatures text [])
    in
        Text text (WebGL.triangles (textMeshHelper Nothing chars 0 0 []))


replaceLigatures : String -> List String -> List String
replaceLigatures st result =
    [ takeLigature 3 st
    , takeLigature 2 st
    , Maybe.map (\( c, rest ) -> ( String.fromChar c, rest )) (String.uncons st)
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.map (\( ch, rest ) -> replaceLigatures rest (ch :: result))
        |> Maybe.withDefault result


takeLigature : Int -> String -> Maybe ( String, String )
takeLigature n st =
    let
        ligature =
            String.left n st
    in
        if String.length ligature == n && Dict.member ligature font then
            Just ( ligature, String.dropLeft n st )
        else
            Nothing


textMeshHelper : Maybe String -> List String -> Float -> Float -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
textMeshHelper prevChar text currentX currentY list =
    case text of
        " " :: rest ->
            textMeshHelper (Just " ") rest (currentX + spaceWidth) currentY list

        "\n" :: rest ->
            textMeshHelper (Just "\n") rest 0 (currentY + emHeight) list

        char :: rest ->
            case Dict.get char font of
                Just charInfo ->
                    addLetter charInfo ( currentX + (letterSpacing prevChar char), currentY ) <|
                        textMeshHelper (Just char) rest (currentX + charInfo.w + (letterSpacing prevChar char)) currentY list

                Nothing ->
                    textMeshHelper prevChar rest currentX currentY list

        [] ->
            list


{-| Returns a spacing between two letters as a sum of:

  - right bearing of the previous letter
  - custom kerning for the pair of letters
  - left bearing of the next letter

-}
letterSpacing : Maybe String -> String -> Float
letterSpacing prevChar nextChar =
    case prevChar of
        Nothing ->
            leftBearing nextChar

        Just " " ->
            leftBearing nextChar

        Just "\n" ->
            leftBearing nextChar

        Just char ->
            List.sum
                [ rightBearing char
                , kerning char nextChar
                , leftBearing nextChar
                ]


addLetter : CharInfo -> ( Float, Float ) -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
addLetter char ( x, y ) =
    [ ( Vertex (vec2 x y) (vec2 char.x char.y)
      , Vertex (vec2 (x + char.w) (y + emHeight)) (vec2 (char.x + char.w) (char.y + emHeight))
      , Vertex (vec2 (x + char.w) y) (vec2 (char.x + char.w) char.y)
      )
    , ( Vertex (vec2 x y) (vec2 char.x char.y)
      , Vertex (vec2 x (y + emHeight)) (vec2 char.x (char.y + emHeight))
      , Vertex (vec2 (x + char.w) (y + emHeight)) (vec2 (char.x + char.w) (char.y + emHeight))
      )
    ]
        |> (++)



-- Shaders


type alias Uniform =
    { offset : Vec3
    , color : Vec3
    , texture : Texture
    , textureSize : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


texturedVertexShader : Shader Vertex Uniform Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        attribute vec2 texPosition;
        uniform vec2 textureSize;
        uniform vec3 offset;
        varying vec2 texturePos;

        void main () {
            vec2 clipSpace = position + offset.xy - 32.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
            texturePos = texPosition / textureSize;
        }

    |]


texturedFragmentShader : Shader {} Uniform Varying
texturedFragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        uniform vec3 color;
        varying vec2 texturePos;

        void main () {
            vec4 textureColor = texture2D(texture, texturePos);
            gl_FragColor = vec4(color, 1.0);
            if (dot(textureColor, textureColor) == 4.0) discard;
        }

    |]
