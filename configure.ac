#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.53])
AC_INIT([GNOME Patience], [3.22.2],
  [http://bugzilla.gnome.org/enter_bug.cgi?product=aisleriot],
  [aisleriot])

# Must run under bash; see bug 726780
if test -z "$BASH_VERSION"; then
  AC_MSG_ERROR([Set CONFIG_SHELL to bash and try again.])
fi

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/sol.c])
AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([1.9 no-dist-gzip dist-bzip2 foreign -Wno-portability])

m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

if test -z "$enable_maintainer_mode"; then
  enable_maintainer_mode=yes
fi
AM_MAINTAINER_MODE([enable])

# *****************************************************************************

GNOME_DEBUG_CHECK

PKG_PROG_PKG_CONFIG([0.15])
PKGS=

AC_PROG_CC
AC_PROG_CPP
AC_PROG_CXX
AC_PROG_LN_S
AC_PROG_MKDIR_P
AC_PROG_SED
AM_PROG_CC_C_O

AC_PROG_LIBTOOL

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

# ******************
# Card theme formats
# ******************

AC_MSG_CHECKING([for requested card theme formats])
AC_ARG_WITH([card-theme-formats],
  [AS_HELP_STRING([--with-card-theme-formats],[which card theme formats to support (svg,fixed,kde,pysol|default|all)])],
  [],[with_card_theme_formats=default])

case "$with_card_theme_formats" in
  default) with_card_theme_formats="svg,pysol" ;;
  all) with_card_theme_formats="svg,kde,fixed,native,pysol" ;;
esac

enable_card_theme_format_svg=no
enable_card_theme_format_kde=no
enable_card_theme_format_fixed=no
enable_card_theme_format_native=no
enable_card_theme_format_pysol=no
need_svg=no
need_rsvg=no
need_qtsvg=no

IFS="${IFS=	}"; gg_saved_ifs="$IFS"; IFS=","
for format in $with_card_theme_formats; do
  case "$format" in
    svg) enable_card_theme_format_svg=yes ;;
    kde) enable_card_theme_format_kde=yes ;;
    fixed) enable_card_theme_format_fixed=yes ;;
    native) enable_card_theme_format_native=yes ;;
    pysol) enable_card_theme_format_pysol=yes ;;
    *) IFS="$gg_saved_ifs" AC_MSG_ERROR([unknown card theme format $format requested]) ;;
  esac
done
IFS="$gg_saved_ifs"

AC_MSG_RESULT([$with_card_theme_formats])

if test "$enable_card_theme_format_svg" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_SVG],[1],[Define to enable SVG card theme format support])
  need_svg=yes
  need_rsvg=yes
fi
if test "$enable_card_theme_format_kde" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_KDE],[1],[Define to enable KDE card theme format support])
  need_svg=yes
  need_qtsvg=yes
fi
if test "$enable_card_theme_format_fixed" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_FIXED],[1],[Define to enable prerendered card theme format support])
fi
if test "$enable_card_theme_format_native" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_NATIVE],[1],[Define to enable native SVG rendering with QtSvg])
  need_svg=yes
  need_qtsvg=yes
fi
if test "$enable_card_theme_format_pysol" = "yes"; then
  AC_DEFINE([ENABLE_CARD_THEME_FORMAT_PYSOL],[1],[Define to enable PySol card theme format support])
fi

AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_SVG],[test "$enable_card_theme_format_svg" = "yes"])
AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_KDE],[test "$enable_card_theme_format_kde" = "yes"])
AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_FIXED],[test "$enable_card_theme_format_fixed" = "yes"])
AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_NATIVE],[test "$enable_card_theme_format_native" = "yes"])
AM_CONDITIONAL([ENABLE_CARD_THEME_FORMAT_PYSOL],[test "$enable_card_theme_format_pysol" = "yes"])

# Card theme paths

AC_MSG_CHECKING([distribution])
if test -f /etc/os-release; then
  DISTRO="$(source /etc/os-release && echo $ID)"
else
  DISTRO="unknown"
fi
AC_MSG_RESULT([$DISTRO])

if test "$enable_card_theme_format_kde" = "yes"; then
  AC_MSG_CHECKING([for KDE card themes base path])
  AC_ARG_WITH([kde-card-theme-path],
    [AS_HELP_STRING([--with-kde-card-theme-path],[The base path to the KDE card themes])],
    [],[case "$DISTRO" in
           debian|opensuse|ubuntu) with_kde_card_theme_path="/usr/share/kde4/apps/carddecks" ;;
	   fedora|rhel|centos) with_kde_card_theme_path="/usr/share/carddecks" ;;
          *) AC_MSG_ERROR([When enabling the KDE card theme format, you must specify the KDE card themes base path.]) ;;
        esac])
  AC_MSG_RESULT([$with_kde_card_theme_path])

  AC_DEFINE_UNQUOTED([KDE_CARD_THEME_PATH],["$with_kde_card_theme_path"],[The base path to the KDE card themes])
fi

if test "$enable_card_theme_format_pysol" = "yes"; then
  AC_MSG_CHECKING([for PySol card themes base path])
  AC_ARG_WITH([pysol-card-theme-path],
    [AS_HELP_STRING([--with-pysol-card-theme-path],[The base path to the PySol card themes])],
    [],[case "$DISTRO" in
          debian|ubuntu) with_pysol_card_theme_path="/usr/share/games/pysol" ;;
          fedora|rhel|centos) with_pysol_card_theme_path="/usr/share/PySolFC" ;;
          opensuse) with_pysol_card_theme_path="/usr/share/games/pysol/data" ;;
          *) AC_MSG_ERROR([When enabling the PySol card theme format, you must specify the PySol card themes base path.]) ;;
        esac])
  AC_MSG_RESULT([$with_pysol_card_theme_path])

  AC_DEFINE_UNQUOTED([PYSOL_CARD_THEME_PATH],["$with_pysol_card_theme_path"],[The base path to the PySol card themes])
fi

# Default card theme

AC_MSG_CHECKING([which card theme to use by default])
AC_ARG_WITH([default-card-theme],
  [AS_HELP_STRING([--with-default-card-theme=NAME],[Which card theme to use by default (default: gnomangelo_bitmap.svgz)])],
  [],[with_default_card_theme="gnomangelo_bitmap.svgz"])
AC_MSG_RESULT([$with_default_card_theme])

AC_DEFINE_UNQUOTED([AR_CARD_THEME_DEFAULT],["$with_default_card_theme"],[The default card theme])

AC_MSG_CHECKING([which card theme format to use by default])
AC_ARG_WITH([default-card-theme-format],
  [AS_HELP_STRING([--with-default-card-theme-format=NAME],[Which card theme format to use by default (default: platform dependent)])],
  [],[with_default_card_theme_format=svg])
AC_MSG_RESULT([$with_default_card_theme_format])

case "$with_default_card_theme_format" in
  svg|kde|fixed|native|pysol) ;;
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
  3.0) GTK_REQUIRED=3.4.0
       RSVG_REQUIRED=2.32.0
       LIBCANBERRA_GTK_REQUIRED=0.26
       LIBCANBERRA_GTK_PKGS="libcanberra-gtk3 >= $LIBCANBERRA_GTK_REQUIRED"
       ;;
esac

GCONF_REQUIRED=2.0
GLIB_REQUIRED=2.32.0
GIO_REQUIRED=2.32.0

# Check for common modules

PKGS="$PKGS gtk+-$GTK_API_VERSION >= $GTK_REQUIRED"

# *****
# Sound
# *****

AC_MSG_CHECKING([whether to enable sound support])
AC_ARG_ENABLE([sound],
  [AS_HELP_STRING([--enable-sound],[Enable sound using libcanberra])],
  [],[enable_sound=yes])
AC_MSG_RESULT([$enable_sound])

if test "$enable_sound" = "yes"; then
  PKGS="$PKGS $LIBCANBERRA_GTK_PKGS"

  AC_DEFINE([ENABLE_SOUND],[1],[Define if sound support is enabled])
fi

AM_CONDITIONAL([ENABLE_SOUND],[test "$enable_sound" = "yes"])

# *********
# GSettings
# *********

GLIB_GSETTINGS

# Check for GNOME modules

m4_ifdef([AM_GCONF_SOURCE_2],
  [AM_GCONF_SOURCE_2
   AC_PATH_PROG([GCONFTOOL],[gconftool-2])
  ],
  [AM_CONDITIONAL([GCONF_SCHEMAS_INSTALL],[false])
   with_platform=gtk-only]
)

if test "$with_platform" = "gnome"; then
  PKGS="$PKGS gconf-2.0 >= $GCONF_REQUIRED"
fi

# Check for librsvg

have_rsvg=no
if test "$need_rsvg" = "yes"; then
  CAIRO_REQUIRED=1.10.0

  PKG_CHECK_EXISTS([librsvg-2.0 >= $RSVG_REQUIRED],[have_rsvg=yes],[have_rsvg=no])

  PKGS="$PKGS librsvg-2.0 >= $RSVG_REQUIRED cairo >= $CAIRO_REQUIRED"

  AC_DEFINE([HAVE_RSVG],[1],[Refine if librsvg is available])
fi

AM_CONDITIONAL([HAVE_RSVG],[test "$have_rsvg" = "yes"])

# Check for QtSvg

have_qtsvg=no
if test "$need_qtsvg" = "yes"; then
  QT_SVG_REQUIRED=5.0.0

  PKG_CHECK_EXISTS([Qt5Svg >= $QT_SVG_REQUIRED],[have_qtsvg=yes],[have_qtsvg=no])

  PKGS="$PKGS Qt5Svg >= $QT_SVG_REQUIRED"
  EXTRA_CXX_LIBS="$EXTRA_CXX_LIBS -lstdc++"

  AC_DEFINE([HAVE_QTSVG],[1],[Refine if QtSvg is available])
fi

AM_CONDITIONAL([HAVE_QTSVG],[test "$have_qtsvg" = "yes"])

if test "$need_svg" = "yes" -a "$have_rsvg" = "no" -a "$have_qtsvg" = "no"; then
   AC_MSG_ERROR([One of librsvg or Qt5Svg are required])
fi

# *****************
# Extra build tools
# *****************

AC_ARG_VAR([GLIB_GENMARSHAL],[the glib-genmarshal programme])
if test -z "$GLIB_GENMARSHAL"; then
  AC_PATH_PROG([GLIB_GENMARSHAL],[glib-genmarshal],[])
  if test -z "$GLIB_GENMARSHAL"; then
    AC_MSG_ERROR([glib-genmarshal not found])
  fi
fi

AC_ARG_VAR([GLIB_COMPILE_RESOURCES],[the glib-compile-resources programme])
if test -z "$GLIB_COMPILE_RESOURCES"; then
  AC_PATH_PROG([GLIB_COMPILE_RESOURCES],[glib-compile-resources],[])
  if test -z "$GLIB_COMPILE_RESOURCES"; then
    AC_MSG_ERROR([glib-compile-resources not found])
  fi
fi

AC_ARG_VAR([GZIP],[the gzip programme])
if test -z "$GZIP"; then
  AC_PATH_PROG([GZIP],[gzip],[])
  if test -z "$GZIP"; then
    AC_MSG_ERROR([gzip not found])
  fi
fi

AC_ARG_VAR([XMLLINT],[the xmllint programme])
if test -z "$XMLLINT"; then
  AC_PATH_PROG([XMLLINT],[xmllint],[])
  if test -z "$XMLLINT"; then
    AC_MSG_ERROR([xmllint not found])
  fi
fi

AC_ARG_VAR([DESKTOP_FILE_VALIDATE],[the desktop-file-validate programme])
if test -z "$DESKTOP_FILE_VALIDATE"; then
  AC_PATH_PROG([DESKTOP_FILE_VALIDATE],[desktop-file-validate],[])
  if test -z "$DESKTOP_FILE_VALIDATE"; then
    AC_MSG_ERROR([desktop-file-validate not found])
  fi
fi

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

# ***********
# Compilation
# ***********

save_CFLAGS="$CFLAGS"
CFLAGS=
CC_CHECK_CFLAGS_APPEND([ \
  -Wall -Wextra \
  -Wformat-nonliteral -Werror=format-security \
  -Wsign-compare -Werror=implicit-function-declaration \
  -Wpointer-arith -Wwrite-strings -Wstrict-prototypes \
  -Waggregate-return -Wcast-align -Wimplicit -Wuninitialized \
  -Wmissing-prototypes -Wmissing-declarations -Wnested-externs \
  -Wpacked -Wmissing-format-attribute -Wshadow -Wlogical-op \
  -Wstrict-aliasing=2 -Winit-self -Wunsafe-loop-optimizations \
  -Wdeclaration-after-statement -Wold-style-definition \
  -Wno-missing-field-initializers -Wno-unused-parameter \
  -Wempty-body -Wformat-signedness \
  -fno-common -fno-strict-aliasing -Wno-switch-enum])
AM_CFLAGS="$AM_CFLAGS $CFLAGS"
CFLAGS="$save_CFLAGS"

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

GUILE_2_0_REQUIRED=2.0.0
GUILE_2_2_REQUIRED=2.1.0

GUILE_PKGS=

AC_MSG_CHECKING([which guile version to use])
AC_ARG_WITH([guile],
  [AS_HELP_STRING([--with-guile=2.0|2.2|auto],[Which guile version to use (default: 2.0)])],
  [case "$with_guile" in
     1.8) AC_MSG_ERROR([unsupported guile version $with_guile requested]) ;;
     2.0|2.2|auto) ;;
     *) AC_MSG_ERROR([unsupported guile version $with_guile requested]) ;;
    esac],
   [with_guile=2.0])

if test "$with_guile" = "auto"; then
  with_guile=
  for version in 2.2 2.0; do
    GUILE_REQUIRED=AS_TR_SH([GUILE_${version}_REQUIRED])
    GUILE_PKGS="guile-$version >= ${!GUILE_REQUIRED}"
    if $PKG_CONFIG --exists $GUILE_PKGS; then
      with_guile=$version
      break
    fi
  done

  if test -z "$with_guile"; then
    AC_MSG_ERROR([no supported guile version found])
  fi

  with_guile_result="$with_guile (auto)"
else
  GUILE_REQUIRED=AS_TR_SH([GUILE_${with_guile}_REQUIRED])
  GUILE_PKGS="guile-$with_guile >= ${!GUILE_REQUIRED}"
  with_guile_result="$with_guile"
fi

AC_MSG_RESULT([$with_guile_result])

PKGS="$PKGS $GUILE_PKGS"

AC_SUBST([GUILE_EFFECTIVE_VERSION],[$with_guile])

AC_ARG_VAR([GUILE],[the guile programme])
if test -z "$GUILE"; then
  AC_PATH_PROG([GUILE],[guile],[false])
  if test "$GUILE" = "false"; then
    AC_MSG_ERROR([guile not found])
  fi
fi

# ************
# Dependencies
# ************

PKG_CHECK_MODULES([AISLERIOT],[$PKGS])
AC_SUBST([AISLERIOT_CFLAGS])
AC_SUBST([AISLERIOT_LIBS])

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

YELP_HELP_INIT([no-lc-media-links no-lc-dist])

AM_CONDITIONAL([BUILD_HELP],[test "$with_help_method" = "ghelp"])

# ********
# Valgrind
# ********

AC_ARG_WITH([valgrind-dir],
  [AC_HELP_STRING([--with-valgrind-dir=PATH],[directory to install Valgrind suppressions to])],
  [],[with_valgrind_dir='${libdir}/valgrind'])
AC_SUBST([valgrinddir],[$with_valgrind_dir])

##############################################

AC_SUBST([AM_CPPFLAGS])
AC_SUBST([AM_CFLAGS])
AC_SUBST([AM_CXXFLAGS])
AC_SUBST([AM_LDFLAGS])
AC_SUBST([EXTRA_CXX_LIBS])

##############################################
##############################################

AC_CONFIG_FILES([
Makefile
cards/Makefile
data/Makefile
data/icons/Makefile
data/sounds/Makefile
data/sol.desktop.in
games/Makefile
help/Makefile
po/Makefile.in
src/Makefile
src/lib/Makefile
src/aisleriot.gresource.xml
])
AC_OUTPUT

echo "
Configuration:

    Source code location:  ${srcdir}
    Compiler:              ${CC}

    Guile version:         ${with_guile_result}
    GTK+ API version:      ${GTK_API_VERSION}
    Platform:              ${with_platform}
    Help method:           ${with_help_method} ${with_help_file_format}
    Using RSVG:            ${have_rsvg}
    Using QtSvg:           ${have_qtsvg}
    Card theme formats:    ${with_card_theme_formats}
    Default theme format:  ${with_default_card_theme_format}
    Default theme:         ${with_default_card_theme}
    Sound support:         ${with_sound}
    Binreloc:              ${enable_binreloc}
"
