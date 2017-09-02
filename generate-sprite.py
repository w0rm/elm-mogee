#!/usr/bin/env python3
"""
Generate a texture from a bunch of sprites
Requires Pillow (pip install Pillow)
"""
import os
import fnmatch
from io import BytesIO
import base64
from PIL import Image

WIDTH = 128
HEIGHT = 64

TEMPLATE = """module View.SpriteData exposing (sprite, spriteSrc, textureSrc, SpriteInfo)

import Dict exposing (Dict)


type alias SpriteInfo =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


sprite : Dict String SpriteInfo
sprite =
    Dict.fromList
%(sprite_info)s


spriteSrc : String
spriteSrc =
    %(sprite_src)s


textureSrc : String
textureSrc =
    %(texture_src)s
"""


def main():
    "The Main Function"
    x_dest = 0
    y_dest = 0
    result_img = Image.new('RGBA', (WIDTH, HEIGHT), (0, 0, 0, 0))
    sprites = []
    max_height = 0
    for root, _, filenames in os.walk('./sprite'):
        for filename in filenames:
            if any(fnmatch.fnmatch(filename, pattern) for pattern in ('*.gif', '*.png')):
                image_path = os.path.join(root, filename)
                img = Image.open(image_path)
                img = img.convert('RGBA')
                width, height = img.size
                max_height = max(max_height, height)
                name, _ = os.path.splitext(os.path.basename(filename))
                if x_dest + width > WIDTH:
                    x_dest = 0
                    y_dest += max_height
                    max_height = 0
                result_img.paste(img, (x_dest, y_dest))
                sprites.append('( "%s", SpriteInfo %d %d %d %d )' % (
                    name, x_dest, y_dest, width, height))
                x_dest += width
    buff = BytesIO()
    result_img.save(buff, 'png')
    sprite_info = ""
    for idx, val in enumerate(sprites):
        if idx == 0:
            sprite_info += "        [ %s\n" % val
        else:
            sprite_info += "        , %s\n" % val
    if sprite_info == "":
        sprite_info = "        [\n"
    sprite_info += "        ]"
    sprite_src = "\"data:image/png;base64,%s\"" % base64.b64encode(
        buff.getvalue()).decode("utf-8")
    buff = BytesIO()
    texture = Image.open("texture.png")
    texture.save(buff, 'png')
    texture_src = "\"data:image/png;base64,%s\"" % base64.b64encode(
        buff.getvalue()).decode("utf-8")
    with open("src/view/SpriteData.elm", "w") as elm_file:
        elm_file.write(TEMPLATE % dict(
            sprite_info=sprite_info,
            texture_src=texture_src,
            sprite_src=sprite_src
        ))


if __name__ == '__main__':
    main()
