#!/usr/bin/env python3
"""
Generate a texture from a bunch of sprites
Requires Pillow and rectpack
"""
import os
import fnmatch
from io import BytesIO
import base64
from PIL import Image
from rectpack import newPacker

WIDTH = 256
HEIGHT = 256

TEMPLATE = """module View.SpriteData exposing (sprite, spriteSrc, SpriteInfo)

import Dict exposing (Dict)


type alias SpriteInfo =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , rotate : Int
    }


sprite : Dict String SpriteInfo
sprite =
    Dict.fromList
%(sprite_info)s


spriteSrc : String
spriteSrc =
    %(sprite_src)s
"""


def main():
    "The Main Function"
    sprites = []

    packer = newPacker()
    packer.add_bin(WIDTH, HEIGHT)

    for root, _, filenames in os.walk('./sprite'):
        for filename in filenames:
            if any(fnmatch.fnmatch(filename, pattern) for pattern in ('*.gif', '*.png')):
                image_path = os.path.join(root, filename)
                img = Image.open(image_path)
                img = img.convert('RGBA')
                width, height = img.size
                name, _ = os.path.splitext(os.path.basename(filename))
                packer.add_rect(width, height, (name, img))

    packer.pack()
    width, height = packer[0].width, packer[0].height
    result_img = Image.new('RGBA', (width, height), (0, 0, 0, 0))
    for rect in packer.rect_list():
        _, x, y, w, h, rid = rect
        name, img = rid
        (imgW, imgH) = img.size
        rotate = (w,h) != (imgW, imgH)
        if rotate:
            img = img.rotate(90, Image.NEAREST, expand = 1)
        result_img.paste(img, (x, y))
        sprites.append('( "%s", SpriteInfo %d %d %d %d %s )' % (name, x, y, imgW, imgH, int(rotate)))

    buff = BytesIO()
    result_img.save(buff, 'png')
    # result_img.save("sheet.png", 'png')
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
    with open("src/view/SpriteData.elm", "w") as elm_file:
        elm_file.write(TEMPLATE % dict(
            sprite_info=sprite_info,
            sprite_src=sprite_src
        ))


if __name__ == '__main__':
    main()
