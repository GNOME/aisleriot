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

. $srcdir/macros/autogen.sh
