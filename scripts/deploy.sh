#!/bin/bash
set -ex

rm -rf dest || exit 0;

mkdir -p dest

# copy assets
cp -r assets dest/

# compile JS using Elm and minify with uglify
elm make src/Main.elm --yes --output dest/assets/elm.js
uglifyjs dest/assets/elm.js -c warnings=false -m --screw-ie8 -o dest/assets/elm.min.js
rm dest/assets/elm.js

# replace elm.js from debug to prod
sed 's/\/_compile\/src\/Main\.elm/assets\/elm\.min\.js/g' index.html > dest/index.html

# steal focus
cat >> dest/index.html <<EOT
<script>
    // Prevent scrolling with arrow and space keys
    window.addEventListener('keydown', function(e) {
        if([32, 37, 38, 39, 40].indexOf(e.keyCode) > -1) {
            e.preventDefault();
        }
    }, false);
    window.focus();
</script>
EOT

# publish to itch.io
./butler push dest unsoundscapes/mogee:html
