#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="Aisleriot"

test -f $srcdir/src/sol.c || {
    echo -n "**Error**: Directory \"\`$srcdir\'\" does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

which gnome-autogen.sh || {
    echo "You need to install gnome-common from GNOME git (or from"
    echo "your OS vendor's package manager)."
    exit 1
}

REQUIRED_AUTOMAKE_VERSION=1.9 
REQUIRED_YELP_TOOLS_VERSION=3.1.1
REQUIRED_GETTEXT_VERSION=0.12
REQUIRED_INTLTOOL_VERSION=0.40.4

. gnome-autogen.sh
