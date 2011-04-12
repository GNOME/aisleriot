#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.53])
AC_INIT([GNOME Patience], [3.1.0+git],
  [http://bugzilla.gnome.org/enter_bug.cgi?product=aisleriot],
  [aisleriot])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/sol.c])
AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([1.9 no-dist-gzip dist-bzip2 foreign])

m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

if test -z "$enable_maintainer_mode"; then
  enable_maintainer_mode=yes
fi
AM_MAINTAINER_MODE([enable])

# *****************************************************************************

PKG_PROG_PKG_CONFIG([0.15])

AC_PROG_CC
AC_PROG_CPP
AC_PROG_LN_S
AC_PROG_SED
AM_PROG_CC_C_O

AC_PROG_LIBTOOL

GNOME_COMMON_INIT
GNOME_DEBUG_CHECK
GNOME_COMPILE_WARNINGS([maximum])
GNOME_CXX_WARNINGS([yes])
GNOME_MAINTAINER_MODE_DEFINES

# *****************
# Distribution info
# *****************

LSB_DISTRIBUTOR=
LSB_DISTRIBUTION=

AC_PATH_PROG([LSB_RELEASE],[lsb_release],[false])
if test "$LSB_RELEASE" != "false"; then
  LSB_DISTRIBUTOR="$($LSB_RELEASE -s -i | sed 's,^\",,;s,\"$,,')"
  LSB_DISTRIBUTION="$($LSB_RELEASE -s -d | sed 's,^\",,;s,\"$,,')"
fi

AC_MSG_CHECKING([for LSB distributor])
AC_MSG_RESULT([$LSB_DISTRIBUTOR])
AC_MSG_CHECKING([for LSB distribution])
AC_MSG_RESULT([$LSB_DISTRIBUTION])

AC_DEFINE_UNQUOTED([LSB_DISTRIBUTOR],["$LSB_DISTRIBUTOR"],[The distributor ID])
AC_DEFINE_UNQUOTED([LSB_DISTRIBUTION],["$LSB_DISTRIBUTION"],[The full distribution description])

# ************
# Dependencies
# ************

# Check which platform to use

AC_MSG_CHECKING([for which platform to build])
AC_ARG_WITH([platform],
  [AS_HELP_STRING([--with-platform=gnome|gtk-only (default: gnome)])],
  [case "$withval" in
    gnome|gtk-only) ;;
    *) AC_MSG_ERROR([invalid argument "$withval" for --with-platform]) ;;
   esac],
  [with_platform=gnome])
AC_MSG_RESULT([$with_platform])

AM_CONDITIONAL([HAVE_GNOME],[test "$with_platform" = "gnome"])

if test "$with_platform" = "gnome"; then
  AC_DEFINE([HAVE_GNOME],[1],[Define if GNOME support is enabled])
fi

# Check which gtk+ major version to use

AC_MSG_CHECKING([which gtk+ version to compile against])
AC_ARG_WITH([gtk],
  [AS_HELP_STRING([--with-gtk=3.0],[which gtk+ version to compile against (default: 3.0)])],
  [case "$with_gtk" in
     3.0) ;;
     [0-9].0) AC_MSG_ERROR([unsupported gtk version $with_gtk specified]) ;;
     *) AC_MSG_ERROR([invalid gtk version specified]) ;;
   esac],
  [with_gtk=3.0])
AC_MSG_RESULT([$with_gtk])

case "$with_gtk" in
  3.0) GTK_API_VERSION=3.0
       GTK_API_MAJOR_VERSION=3
       ;;
esac

AC_SUBST([GTK_API_VERSION])
AC_SUBST([GTK_API_MAJOR_VERSION])

# **************
# Win32 platform
# **************

AC_CANONICAL_HOST

AC_MSG_CHECKING([for some Win32 platform])
case "$host" in
  *-*-cygwin*|*-*-mingw*)
    platform_win32=yes
    ;;
  *)
    platform_win32=no
    ;;
esac
AC_MSG_RESULT([$platform_win32])
AM_CONDITIONAL([PLATFORM_WIN32],[test "$platform_win32" = "yes"])

AC_MSG_CHECKING([for native Win32])
case "$host" in
  *-*-mingw*)
    os_win32=yes
    ;;
  *)
    os_win32=no
    ;;
esac
AC_MSG_RESULT([$os_win32])
AM_CONDITIONAL([PLATFORM_WIN32_NATIVE],[test "$os_win32" = "yes"])

# ********
# Features
# ********

# *****
# Sound
# *****

AC_MSG_CHECKING([whether to enable sound support])
AC_ARG_ENABLE([sound],
  [AS_HELP_STRING([--enable-sound],[Enable sound using libcanberra])],
  [],[enable_sound=yes])
AC_MSG_RESULT([$enable_sound])

# *******
# Clutter
# *******

AC_MSG_CHECKING([whether to enable clutter])
AC_ARG_WITH([clutter],
  [AS_HELP_STRING([--with-clutter],[Enable the clutter version of aisleriot (default: disabled)])],
  [],[with_clutter=no])
AC_MSG_RESULT([$with_clutter])

if test "$with_clutter" = "yes"; then
  # Distro packagers: DO NOT ENABLE AISLERIOT/CLUTTER IN YOUR DISTRO PACKAGES, OR ELSE!
  AC_MSG_NOTICE([Aisleriot/Clutter is experimental; do not enable this for distribution packages!])
fi

# ******************
# Card theme formats
# ******************

AC_MSG_CHECKING([for requested card theme formats])
AC_ARG_WITH([card-theme-formats],
  [AS_HELP_STRING([--with-card-theme-formats],[which card theme formats to support (svg,fixed,kde,pysol|default|all)])],
  [],[with_card_theme_formats=default])

case "$with_card_theme_formats" in
  default) with_card_theme_formats="svg" ;;
  all) with_card_theme_formats="svg,kde,fixed,sliced,pysol" ;;
esac

enable_card_theme_format_svg=no
enable_card_theme_format_kde=no
enable_card_theme_format_fixed=no
enable_card_theme_format_sliced=no
enable_card_theme_format_pysol=no
need_rsvg=no

IFS="${IFS=	}"; gg_saved_ifs="$IFS"; IFS=","
for format in $with_card_theme_formats; do
  case "$format" in
    svg) enable_card_theme_format_svg=yes ;;
    kde) enable_card_theme_format_kde=yes ;;
    fixed) enable_card_theme_format_fixed=yes ;;
    sliced) enable_card_theme_format_svg=yes ;;
    pysol) enable_card_theme_format_pysol=yes ;;
    *) IFS="$gg_saved_ifs" AC_MSG_ERROR([unknown card theme format $format requested]) ;;
  esac
done
IFS="$gg_saved_ifs"

AC_MSG_RESULT([$with_card_theme_formats])

if test "$enable_card_theme_format_svg" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_SVG],[1],[Define to enable SVG card theme format support])
  need_rsvg=yes
fi
if test "$enable_card_theme_format_kde" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_KDE],[1],[Define to enable KDE card theme format support])
  need_rsvg=yes
fi
if test "$enable_card_theme_format_fixed" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_FIXED],[1],[Define to enable prerendered card theme format support])
fi
if test "$enable_card_theme_format_sliced" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_SLICED],[1],[Define to enable pre-2.19 card theme format support])
fi
if test "$enable_card_theme_format_pysol" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_PYSOL],[1],[Define to enable PySol card theme format support])
fi

AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_SVG],[test "$enable_card_theme_format_svg" = "yes"])
AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_KDE],[test "$enable_card_theme_format_kde" = "yes"])
AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_FIXED],[test "$enable_card_theme_format_fixed" = "yes"])
AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_SLICED],[test "$enable_card_theme_format_sliced" = "yes"])
AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_PYSOL],[test "$enable_card_theme_format_pysol" = "yes"])

# Card theme paths

if test "$enable_card_theme_format_kde" = "yes"; then
  AC_MSG_CHECKING([for KDE card themes base path])
  AC_ARG_WITH([kde-card-theme-path],
    [AS_HELP_STRING([--with-kde-card-theme-path],[The base path to the KDE card themes])],
    [],[case $LSB_DISTRIBUTION in
          *Debian*|*Fedora*|*RedHat*|*SUSE*|*Ubuntu*) with_kde_card_theme_path="/usr/share/kde4/apps/carddecks" ;;
          *) AC_MSG_ERROR([When enabling the KDE card theme format, you must specify the KDE card themes base path.]) ;;
        esac])
  AC_MSG_RESULT([$with_kde_card_theme_path])

  AC_DEFINE_UNQUOTED([KDE_CARD_THEME_PATH],["$with_kde_card_theme_path"],[The base path to the KDE card themes])
fi

if test "$enable_card_theme_format_pysol" = "yes"; then
  AC_MSG_CHECKING([for PySol card themes base path])
  AC_ARG_WITH([pysol-card-theme-path],
    [AS_HELP_STRING([--with-pysol-card-theme-path],[The base path to the PySol card themes])],
    [],[case $LSB_DISTRIBUTION in
          *Debian*|*Ubuntu*) with_pysol_card_theme_path="/usr/share/games/pysol" ;;
          *Fedora*|*RedHat*) with_pysol_card_theme_path="/usr/share/PySolFC" ;;
          *SUSE*)            with_pysol_card_theme_path="/usr/share/games/pysol/data" ;;
          *) AC_MSG_ERROR([When enabling the PySol card theme format, you must specify the PySol card themes base path.]) ;;
        esac])
  AC_MSG_RESULT([$with_pysol_card_theme_path])

  AC_DEFINE_UNQUOTED([PYSOL_CARD_THEME_PATH],["$with_pysol_card_theme_path"],[The base path to the PySol card themes])
fi

# Default card theme

AC_MSG_CHECKING([which card theme to use by default])
AC_ARG_WITH([default-card-theme],
  [AS_HELP_STRING([--with-default-card-theme=NAME],[Which card theme to use by default (default: bonded.svg)])],
  [],[with_default_card_theme="bonded.svg"])
AC_MSG_RESULT([$with_default_card_theme])

AC_DEFINE_UNQUOTED([AR_CARD_THEME_DEFAULT],["$with_default_card_theme"],[The default card theme])

AC_MSG_CHECKING([which card theme format to use by default])
AC_ARG_WITH([default-card-theme-format],
  [AS_HELP_STRING([--with-default-card-theme-format=NAME],[Which card theme format to use by default (default: platform dependent)])],
  [],[with_default_card_theme_format=svg])
AC_MSG_RESULT([$with_default_card_theme_format])

case "$with_default_card_theme_format" in
  svg|kde|fixed|sliced|pysol) ;;
  *) AC_MSG_ERROR([card theme format $with_default_card_theme_format unknown])
esac

AC_DEFINE_UNQUOTED([AR_CARD_THEME_DEFAULT_FORMAT_STRING],["$with_default_card_theme_format"],[The default card theme format type string])

# ******************
# Extra Debugging UI
# ******************

AC_MSG_CHECKING([whether extra debugging UI in Aisleriot is requested])
AC_ARG_ENABLE([debug-ui],
  [AS_HELP_STRING([--debug-ui],[Enable extra debugging UI in Aisleriot (default: disabled)])],
  [],[enable_debug_ui=no])
AC_MSG_RESULT([$enable_debug_ui])

if test "$enable_debug_ui" = "yes"; then
  AC_DEFINE([ENABLE_DEBUG_UI],[1],[Define extra debugging UI in Aisleriot is enabled])
fi

AM_CONDITIONAL([ENABLE_DEBUG_UI],[test "$enable_debug_ui" = "yes"])

# ********************
# Checks for libraries
# ********************

case "$with_gtk" in
  3.0) GTK_REQUIRED=3.0.0
       RSVG_REQUIRED=2.32.0
       LIBCANBERRA_GTK_REQUIRED=0.26
       LIBCANBERRA_GTK_PKGS="libcanberra-gtk3 >= $LIBCANBERRA_GTK_REQUIRED"
       ;;
esac

GCONF_REQUIRED=2.0
GLIB_REQUIRED=2.26.0
GIO_REQUIRED=2.26.0
GSTREAMER_REQUIRED=0.10.11

# Check for common modules

need_gthread=no
PKG_CHECK_MODULES([GTHREAD],[gthread-2.0])
AC_SUBST([GTHREAD_CFLAGS])
AC_SUBST([GTHREAD_LIBS])

PKG_CHECK_MODULES([GMODULE],[gmodule-2.0])
AC_SUBST([GMODULE_CFLAGS])
AC_SUBST([GMODULE_LIBS])

PKG_CHECK_MODULES([GOBJECT],[gobject-2.0])
AC_SUBST([GOBJECT_CFLAGS])
AC_SUBST([GOBJECT_LIBS])

PKG_CHECK_MODULES([GTK],[gtk+-$GTK_API_VERSION >= $GTK_REQUIRED])
AC_SUBST([GTK_CFLAGS])
AC_SUBST([GTK_LIBS])

# *********
# GSettings
# *********

GLIB_GSETTINGS

# Check for GNOME modules

AM_GCONF_SOURCE_2
AC_PATH_PROG([GCONFTOOL],[gconftool-2])

if test "$with_platform" = "gnome"; then
  PKG_CHECK_MODULES([GNOME],[gconf-2.0 >= $GCONF_REQUIRED])
  AC_SUBST([GNOME_CFLAGS])
  AC_SUBST([GNOME_LIBS])

  need_gthread=yes
fi

# Check for librsvg

have_rsvg=no
if test "$need_rsvg" = "yes"; then
  have_rsvg=yes

  CAIRO_REQUIRED=1.10.0

  # Errors out if rsvg is not found
  PKG_CHECK_MODULES([RSVG],[
    librsvg-2.0 >= $RSVG_REQUIRED
    cairo >= $CAIRO_REQUIRED])

  AC_SUBST([RSVG_CFLAGS])
  AC_SUBST([RSVG_LIBS])

  AC_DEFINE([HAVE_RSVG],[1],[Refine if librsvg is available])
fi

AM_CONDITIONAL([HAVE_RSVG],[test "$have_rsvg" = "yes"])

# Check for Clutter

if test "$with_clutter" = "yes"; then
  CLUTTER_REQUIRED=1.0.0

  PKG_CHECK_MODULES([CLUTTER],[clutter-1.0 >= $CLUTTER_REQUIRED])
  AC_SUBST([CLUTTER_CFLAGS])
  AC_SUBST([CLUTTER_LIBS])

  case "$with_gtk" in
    3.0) CLUTTER_GTK_API_VERSION=1.0
         CLUTTER_GTK_REQUIRED=0.91.6
         ;;
  esac

  PKG_CHECK_MODULES([CLUTTER_GTK],[clutter-gtk-$CLUTTER_GTK_API_VERSION >= $CLUTTER_GTK_REQUIRED])
  AC_SUBST([CLUTTER_GTK_CFLAGS])
  AC_SUBST([CLUTTER_GTK_LIBS])
fi

AM_CONDITIONAL([HAVE_CLUTTER],[test "$with_clutter" = "yes"])

# GThread

AM_CONDITIONAL([WITH_GTHREAD],[test "$need_gthread" = "yes"])

# libcanberra

if test "$enable_sound" = "yes"; then
  PKG_CHECK_MODULES([CANBERRA_GTK],[$LIBCANBERRA_GTK_PKGS],[],
    [AC_MSG_ERROR([Sound support was requested, but libcanberra-gtk not found.])])

  AC_DEFINE([ENABLE_SOUND],[1],[Define if sound support is enabled])
fi

AM_CONDITIONAL([ENABLE_SOUND],[test "$enable_sound" = "yes"])

# SMClient

SMCLIENT_PKGS=

AC_MSG_CHECKING([for GDK target])
case "$with_gtk" in
  3.0) for target in $($PKG_CONFIG --variable targets gdk-$GTK_API_VERSION); do
          GDK_TARGET="$target"
          break
        done
        ;;
esac
AC_MSG_RESULT([$GDK_TARGET])

case "$GDK_TARGET" in
  x11|win32|quartz) ;;
  *) AC_MSG_ERROR([unknown GDK target])
esac

AC_MSG_CHECKING([which smclient backend to use])
AC_ARG_WITH([smclient],
  [AS_HELP_STRING([--with-smclient],[which smclient backend to use (xsmp|win32|quartz|no)])],
  [],
  [case "$GDK_TARGET" in
     x11) with_smclient=xsmp SMCLIENT_PKGS="sm ice" ;;
     win32|quartz) with_smclient=$GDK_TARGET ;;
     *) with_smclient=no ;;
   esac])
AC_MSG_RESULT([$with_smclient])

if test "$with_smclient" != "no"; then
  if test -n "$SMCLIENT_PKGS"; then
    PKG_CHECK_MODULES([SMCLIENT],[$SMCLIENT_PKGS])
  fi

  AC_DEFINE([WITH_SMCLIENT],[1],[Define if smclient is enabled])
fi

AM_CONDITIONAL([WITH_SMCLIENT],[test "$with_smclient" != "no"])
AM_CONDITIONAL([WITH_SMCLIENT_XSMP],[test "$with_smclient" = "xsmp"])
AM_CONDITIONAL([WITH_SMCLIENT_WIN32],[test "$with_smclient" = "win32"])
AM_CONDITIONAL([WITH_SMCLIENT_QUARTZ],[test "$with_smclient" = "quartz"])

# *****************
# Extra build tools
# *****************

AC_PATH_PROG([GLIB_GENMARSHAL],[glib-genmarshal])

if test "$platform_win32" = "yes" -a "$os_win32" = "yes"; then
  AC_CHECK_TOOL([WINDRES],[windres],[no])
  if test "$WINDRES" = "no"; then
    AC_MSG_ERROR([windres is required for native win32])
  fi
fi

# ********
# Binreloc
# ********

AC_MSG_CHECKING([whether to enable binary relocation support])
AC_ARG_ENABLE([binreloc],
  [AS_HELP_STRING([--enable-binreloc],[enable binary relocation support (default: disabled)])],
  [],
  [enable_binreloc="$platform_win32"])
AC_MSG_RESULT([$enable_binreloc])

if test "$enable_binreloc" = "yes"; then

  # Check that all variables use the same prefix
  # Note: datarootdir exists only since autoconf 2.60, so we have to
  # check for the old and the new form of datadir.
  if test "$exec_prefix" != '${prefix}' -o \
          "$bindir" != '${exec_prefix}/bin' -o \
          "$sbindir" != '${exec_prefix}/sbin' -o \
          "$libdir" != '${exec_prefix}/lib' -o \
          "$libexecdir" != '${exec_prefix}/libexec' -o \
          "$sysconfdir" != '${prefix}/etc' -o \
          "$localstatedir" != '${prefix}/var' -o \
          \( -n "$datarootdir" -a "$datarootdir" != '${prefix}/share' \) -o \
          \( "$datadir" != '${datarootdir}' -a "$datadir" != '${prefix}/share' \) -o \
          \( "$localedir" != '${datarootdir}/locale' -a "$localedir" != '${datadir}/locale' \) -o \
          \( "$mandir" != '${datarootdir}/man' -a "$mandir" != '${datadir}/man' \); then
    AC_MSG_ERROR([cannot use binary relocation with different prefixes])
  fi

  AC_DEFINE([ENABLE_BINRELOC],[1],[Define for binary relocation support])
fi

# ********

AM_CFLAGS="$AM_CFLAGS $WARN_CFLAGS"
AM_CXXFLAGS="$AM_CXXFLAGS $WARN_CXXFLAGS"

# ****
# i18n
# ****

GETTEXT_PACKAGE=aisleriot
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE", [The gettext package name])
AM_GLIB_GNU_GETTEXT

IT_PROG_INTLTOOL([0.35.0])

# *****
# Guile
# *****

GUILE_1_8_REQUIRED=1.8.0

PKG_CHECK_MODULES([GUILE],[guile-1.8 >= $GUILE_1_8_REQUIRED])

# *************
# Documentation
# *************

# We support various ways to present help to the user:
# ghelp: using the ghelp: protocol, most likely displaying in Yelp
# file: showing html or xhtml files in the web browser
# library: in the web browser loaded remotedly from library.gnome.org
#
# Note that for help using [x]html files, we can't use gnome-doc-utils.make
# since it doesn't currently support this. The packager will have to use
# gnome-doc-tool directly to build the documentation in the right format.
# The help files must be installed in $(pkgdatadir)/$(DOC_MODULE)/$(LOCALE) .

AC_MSG_CHECKING([which help method to use])
AC_ARG_WITH([help-method],
  [AS_HELP_STRING([--with-help-method],[which help method to use (ghelp|file|library; default: ghelp)])],
  [],
  [if test "$platform_win32" = "yes"; then
     with_help_method=file
   else
     with_help_method=ghelp
   fi])
AC_MSG_RESULT([$with_help_method])

case "$with_help_method" in
  ghelp) AC_DEFINE([WITH_HELP_METHOD_GHELP],[1],[Define to use help using ghelp]) ;;
  file) AC_DEFINE([WITH_HELP_METHOD_FILE],[1],[Define to use help using file]) ;;
  library) AC_DEFINE([WITH_HELP_METHOD_LIBRARY],[1],[Define to use help using library.gnome.org]) ;;
  *) AC_MSG_ERROR([unknown help method "$with_help_method"]) ;;
esac

if test "$with_help_method" = "file"; then
  AC_MSG_CHECKING([for help file format])
  AC_ARG_WITH([help-file-format],
    [AS_HELP_STRING([--with-help-file-format],[which file format to use for help (html|xhtml; default: html)])],
    [case "$with_help_file_format" in
       html|xhtml) ;;
       *) AC_MSG_ERROR([unknown help file format "$with_help_file_format"]) ;;
     esac],
    [with_help_file_format=html])
  AC_MSG_RESULT([$with_help_file_format])

  AC_DEFINE_UNQUOTED([HELP_FILE_FORMAT],["$with_help_file_format"],[The help file format])
fi

GNOME_DOC_INIT([0.10.0],
  [have_gdu=yes],
  [have_gdu=no
   if test "$with_help_method" = "ghelp"; then
     AC_MSG_ERROR([ghelp requested, but gnome-doc-utils not found!])
   fi])

AM_CONDITIONAL([BUILD_HELP],[test "$with_help_method" = "ghelp"])

##############################################

AC_SUBST([AM_CPPFLAGS])
AC_SUBST([AM_CFLAGS])
AC_SUBST([AM_CXXFLAGS])
AC_SUBST([AM_LDFLAGS])

##############################################
##############################################

AC_CONFIG_FILES([
Makefile
cards/Makefile
data/Makefile
data/icons/Makefile
data/sounds/Makefile
data/sol.desktop.in
data/freecell.desktop.in
games/Makefile
help/Makefile
po/Makefile.in
src/Makefile
src/lib/Makefile
src/smclient/Makefile
])
AC_OUTPUT

echo "
Configuration:

    Source code location:  ${srcdir}
    Compiler:              ${CC}

    Platform:              ${with_platform}"

if test -n "$with_platform_variant"; then
  echo "
    Platform variant:      ${with_platform_variant}"
fi

echo "

    GTK+ API version:      ${GTK_API_VERSION}
    Help method:           ${with_help_method} ${with_help_file_format}
    Using SM Client:       ${with_smclient}
    Using RSVG:            ${have_rsvg}
    Card theme formats:    ${with_card_theme_formats}
    Default theme format:  ${with_default_card_theme_format}
    Default theme:         ${with_default_card_theme}
    Sound support:         ${with_sound}
    Binreloc:              ${enable_binreloc}
"
