#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="Gnome Games"

(test -f $srcdir/configure.in \
  && test -d $srcdir/gnomine \
  && test -d $srcdir/same-gnome) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level gnome directory"
    exit 1
}

which gnome-autogen.sh || {
    echo "You need to install gnome-common from the GNOME CVS"
    exit 1
}

ACLOCAL_FLAGS="-I $srcdir/games-macros $ACLOCAL_FLAGS"
USE_GNOME2_MACROS=1 . gnome-autogen.sh
