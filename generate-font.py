#!/usr/bin/env python3
"""
Generate a font texture from a bunch of chars
Requires Pillow (pip install Pillow)
"""
import os
import fnmatch
from PIL import Image

SIZE = 64
DEST_PATH = os.path.realpath('./font.png')
LINE_HEIGHT = 11

def main():
    "The Main Function"
    x_dest = 0
    y_dest = 0
    result_img = Image.new('RGBA', (SIZE, SIZE))
    for root, _, filenames in os.walk('./font'):
        for filename in fnmatch.filter(filenames, '*.gif'):
            image_path = os.path.join(root, filename)
            img = Image.open(image_path)
            width, _ = img.size
            name, _ = os.path.splitext(os.path.basename(filename))
            if x_dest + width > SIZE:
                x_dest = 0
                y_dest += LINE_HEIGHT
            result_img.paste(img, (x_dest, y_dest))
            print(", ('%s', CharInfo %d %d %d)" % (name, x_dest, y_dest, width))
            x_dest += width

    result_img.save(DEST_PATH)

if __name__ == '__main__':
    main()
