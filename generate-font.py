#!/usr/bin/env python3
"""
Generate a font texture from a bunch of chars
Requires Pillow (pip install Pillow)
"""
import os
import fnmatch
from io import BytesIO
import base64
from PIL import Image

WIDTH = 128
HEIGHT = 64
DEST_PATH = os.path.realpath('./font.png')
LINE_HEIGHT = 11


TEMPLATE = """module View.FontData exposing (font, fontSrc, CharInfo)

import Dict exposing (Dict)


type alias CharInfo =
    { x : Float
    , y : Float
    , w : Float
    }


font : Dict String CharInfo
font =
    Dict.fromList
%(char_info)s


fontSrc : String
fontSrc =
    %(font_src)s
"""

def main():
    "The Main Function"
    x_dest = 0
    y_dest = 0
    result_img = Image.new('RGB', (WIDTH, HEIGHT), (255, 255, 255))
    chars = []
    for root, _, filenames in os.walk('./font'):
        for filename in filenames:
            if any(fnmatch.fnmatch(filename, pattern) for pattern in ('*.gif', '*.png')):
                image_path = os.path.join(root, filename)
                img = Image.open(image_path)
                width, _ = img.size
                name, _ = os.path.splitext(os.path.basename(filename))
                name = "".join([chr(int(n, 16)) for n in name.split("_")])
                name = name.replace('\\', '\\\\').replace('\"', '\\\"')
                if x_dest + width > WIDTH:
                    x_dest = 0
                    y_dest += LINE_HEIGHT
                result_img.paste(img, (x_dest, y_dest))
                chars.append('( "%s", CharInfo %d %d %d )' % (name, x_dest, y_dest, width))
                x_dest += width
    buff = BytesIO()
    result_img = result_img.convert('1')
    result_img.save(buff, 'png')
    char_info = ""
    for idx, val in enumerate(chars):
        if idx == 0:
            char_info += "        [ %s\n" % val
        else:
            char_info += "        , %s\n" % val
    char_info += "        ]"
    font_src = "\"data:image/png;base64,%s\"" % base64.b64encode(buff.getvalue()).decode("utf-8")
    with open("src/view/FontData.elm", "w") as f:
        f.write(TEMPLATE % dict(char_info=char_info, font_src=font_src))
    # result_img.save(DEST_PATH, "png")


if __name__ == '__main__':
    main()
