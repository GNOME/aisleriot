#!/bin/sh

xgettext --default-domain=gnome-games --directory=.. \
  --add-comments --keyword=_ --keyword=N_ \
  --files-from=./POTFILES.in \
&& test ! -f gnome-games.po \
   || ( rm -f ./gnome-games.pot \
    && mv gnome-games.po ./gnome-games.pot )
