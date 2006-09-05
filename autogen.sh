#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="Gnome Games"

(test -f $srcdir/configure.in \
  && test -d $srcdir/gnomine \
  && test -d $srcdir/same-gnome) || {
    echo -n "**Error**: Directory \"\`$srcdir\'\" does not look like the"
    echo " top-level gnome directory"
    exit 1
}

which gnome-autogen.sh || {
    echo "You need to install gnome-common from the GNOME CVS"
    exit 1
}

REQUIRED_AUTOMAKE_VERSION=1.7.2

# Dont't run configure yet, ...
GNM_NOCONFIGURE=$NOCONFIGURE
NOCONFIGURE=1
USE_GNOME2_MACROS=1 ACLOCAL_FLAGS="-I m4 $ACLOCAL_FLAGS" . gnome-autogen.sh

# Use own copy of Makefile.in.in for gnome-games.
# See po/README.TRANSLATORS for more information.
echo "Creating po/Makefile.in.in"
rm -f $srcdir/po/Makefile.in.in 
cp $srcdir/po/Makefile.in.in.own $srcdir/po/Makefile.in.in

# ... and then proceed.
if test "x$GNM_NOCONFIGURE" = x; then
    printbold Running $srcdir/configure $conf_flags "$@" ...
    $srcdir/configure $conf_flags "$@" \
         || exit 1
fi



