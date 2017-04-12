#!/bin/bash
set -e

rm -rf dest || exit 0;

mkdir -p dest

cp -r snd dest/

# compile JS using Elm
elm make src/Main.elm --yes --output dest/index.html

# replace ../snd/theme.ogg with snd/theme.ogg
sed 's/\.\.\/snd\/theme.ogg/snd\/theme.ogg/g' dest/index.html > dest/index.html.tmp && mv dest/index.html.tmp dest/index.html

# publish to itch.io
./butler push dest unsoundscapes/mogee:html
