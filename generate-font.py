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

SIZE = 64
DEST_PATH = os.path.realpath('./font.png')
LINE_HEIGHT = 11

def main():
    "The Main Function"
    x_dest = 0
    y_dest = 0
    result_img = Image.new('RGB', (SIZE, SIZE), (255, 255, 255))
    for root, _, filenames in os.walk('./font'):
        for filename in filenames:
            if any(fnmatch.fnmatch(filename, pattern) for pattern in ('*.gif', '*.png')):
                image_path = os.path.join(root, filename)
                img = Image.open(image_path)
                width, _ = img.size
                name, _ = os.path.splitext(os.path.basename(filename))
                name = chr(int(name, 16))
                if name == '\'':
                    name = '\\\''
                if x_dest + width > SIZE:
                    x_dest = 0
                    y_dest += LINE_HEIGHT
                result_img.paste(img, (x_dest, y_dest))
                print(", ('%s', CharInfo %d %d %d)" % (name, x_dest, y_dest, width))
                x_dest += width
    buff = BytesIO()
    result_img = result_img.convert('1')
    result_img.save(buff, 'png')
    print("data:image/png;base64,%s" % base64.b64encode(buff.getvalue()).decode("utf-8"))
    result_img.save(DEST_PATH, 'png')

if __name__ == '__main__':
    main()
