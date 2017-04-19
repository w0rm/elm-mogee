#!/bin/bash
set -e

rm -rf dest || exit 0;

mkdir -p dest

cp -r snd dest/

# compile JS using Elm
elm make src/Main.elm --yes --output dest/index.html

cd dest

# replace ../snd/theme.ogg with snd/theme.ogg
sed 's/\.\.\/snd\/theme.ogg/snd\/theme.ogg/g' index.html > index.html.tmp && mv index.html.tmp index.html

# steal focus
cat >> index.html <<EOT
<script>
   window.focus();
</script>
EOT

# publish to itch.io
butler push ../dest unsoundscapes/mogee:html
