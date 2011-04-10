#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.53])
AC_INIT([GNOME Patience], [3.0.0],
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

# Feature matrix

need_cxx=no
need_vala=no
need_rsvg=yes
need_guile=yes
need_clutter=no
need_gio_2_26=no
want_sound=yes
allow_gtk2=yes
allow_gnuchess=no
allow_smclient=yes
disallow_hildon=no
require_gtk_2_16=no
require_cairo_1_10=no

# Locate various programs

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
  [AS_HELP_STRING([--with-platform=gnome|hildon|gtk-only (default: gnome)])],
  [case "$withval" in
    gnome|hildon|gtk-only) ;;
    maemo) with_platform=hildon with_platform_variant=maemo3 ;; # compat
    *) AC_MSG_ERROR([invalid argument "$withval" for --with-platform]) ;;
   esac],
  [with_platform=gnome])
AC_MSG_RESULT([$with_platform])

if test "$with_platform" = "hildon"; then
  # "maemo" is Maemo 4.x
  AC_MSG_CHECKING([for which platform variant to build])
  AC_ARG_WITH([platform-variant],
    [AS_HELP_STRING([--with-platform-variant=maemo3|maemo4|maemo5|mid (default: maemo4)])],
    [case "$withval" in
       maemo5|maemo4|maemo3|mid) ;;
       *) with_platform_variant=maemo4 ;;
     esac],
    [with_platform_variant=maemo3])
  AC_MSG_RESULT([$with_platform_variant])
fi

if test "$with_platform" = "hildon" -a "$disallow_hildon" = "yes"; then
  AC_MSG_ERROR([Trying to build with Hildon platform, but some of the following games don't support this: $gamelist])
fi

AM_CONDITIONAL([HAVE_GNOME],[test "$with_platform" = "gnome"])
AM_CONDITIONAL([HAVE_HILDON],[test "$with_platform" = "hildon"])
AM_CONDITIONAL([HAVE_MAEMO],[test "$with_platform" = "hildon" -a "$with_platform_variant" != "mid"])
AM_CONDITIONAL([HAVE_MAEMO_3],[test "$with_platform" = "hildon" -a "$with_platform_variant" = "maemo3"])
AM_CONDITIONAL([HAVE_MAEMO_4],[test "$with_platform" = "hildon" -a "$with_platform_variant" = "maemo4"])
AM_CONDITIONAL([HAVE_MAEMO_5],[test "$with_platform" = "hildon" -a "$with_platform_variant" = "maemo5"])

if test "$with_platform" = "gnome"; then
  AC_DEFINE([HAVE_GNOME],[1],[Define if GNOME support is enabled])
fi
if test "$with_platform" = "hildon"; then
  AC_DEFINE([HAVE_HILDON],[1],[Define if Hildon support is enabled])

  if test "$with_platform_variant" != "mid"; then
    AC_DEFINE([HAVE_MAEMO],[1],[Define if Maemo support is enabled])
  fi
  if test "$with_platform_variant" = "maemo3"; then
    AC_DEFINE([HAVE_MAEMO_3],[1],[Define if Maemo 3.x support is enabled])
  fi
  if test "$with_platform_variant" = "maemo4"; then
    AC_DEFINE([HAVE_MAEMO_4],[1],[Define if Maemo 4.x support is enabled])
  fi
  if test "$with_platform_variant" = "maemo5"; then
    AC_DEFINE([HAVE_MAEMO_5],[1],[Define if Maemo 5.x support is enabled])
  fi
fi

# Check which gtk+ major version to use

AC_MSG_CHECKING([which gtk+ version to compile against])
AC_ARG_WITH([gtk],
  [AS_HELP_STRING([--with-gtk=2.0|3.0],[which gtk+ version to compile against (default: 3.0)])],
  [case "$with_gtk" in
     2.0|3.0) ;;
     *) AC_MSG_ERROR([invalid gtk version specified]) ;;
   esac],
  [with_gtk=3.0])
AC_MSG_RESULT([$with_gtk])

if test "$with_gtk" = "2.0" -a "$allow_gtk2" = "no"; then
  AC_MSG_ERROR([gtk+-2.0 build requested but not all selected games still support this])
fi

case "$with_gtk" in
  2.0) GTK_API_VERSION=2.0
       GTK_API_MAJOR_VERSION=2
       ;;
  3.0) GTK_API_VERSION=3.0
       GTK_API_MAJOR_VERSION=3
       ;;
esac

AC_SUBST([GTK_API_VERSION])
AC_SUBST([GTK_API_MAJOR_VERSION])

AM_CONDITIONAL([HAVE_GTK_2],[test "$GTK_API_VERSION" = "2.0"])
AM_CONDITIONAL([HAVE_GTK_3],[test "$GTK_API_VERSION" = "3.0"])

# Win32 platform

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

if test "$want_sound" = "yes"; then
  AC_MSG_CHECKING([whether to enable sound support])
  AC_ARG_ENABLE([sound],
    [AS_HELP_STRING([--enable-sound],[Enable sound using libcanberra])],
    [],[case "$with_platform" in
          gnome|gtk-only) enable_sound=yes ;;
          hildon) case "$with_platform_variant" in
                    maemo5) enable_sound=yes ;;
                    *) enable_sound=no ;;
                  esac
        esac])
  AC_MSG_RESULT([$enable_sound])
else
  enable_sound=no
fi

# *******
# Clutter
# *******

# We're using need_guile as a quick way to check whether building aisleriot
if test "$need_guile" = "yes"; then
  AC_MSG_CHECKING([whether to enable src/clutter])
  AC_ARG_ENABLE([aisleriot-clutter],
    [AS_HELP_STRING([--enable-aisleriot-clutter],[Whether to enable the clutter version of aisleriot (default: disabled)])],
    [],[enable_aisleriot_clutter=no])
  AC_MSG_RESULT([$enable_aisleriot_clutter])

  if test "$enable_aisleriot_clutter" = "yes"; then
    # Distro packagers: DO NOT ENABLE AISLERIOT/CLUTTER IN YOUR DISTRO PACKAGES, OR ELSE!
    AC_MSG_NOTICE([Aisleriot/Clutter is experimental; do not enable this for distribution packages!])
    need_clutter=yes
  fi
fi

AM_CONDITIONAL([ENABLE_AISLERIOT_CLUTTER],[test "$enable_aisleriot_clutter" = "yes"])

case "$with_platform" in
  gnome|gtk-only) ;;
  hildon) if test "$need_clutter" = "yes"; then
            AC_MSG_ERROR([Clutter is not supported on hildon; disable Gnometris, Lights Off and Aisleriot/Clutter])
          fi
          ;;
esac

# ******************
# Card theme formats
# ******************

AC_MSG_CHECKING([for requested card theme formats])
AC_ARG_WITH([card-theme-formats],
  [AS_HELP_STRING([--with-card-theme-formats],[which card theme formats to support (svg,fixed,kde,pysol|default|all)])],
  [],[with_card_theme_formats=default])

case "$with_card_theme_formats" in
  default) case "$with_platform" in
             gnome|gtk-only) with_card_theme_formats="svg" ;;
             hildon) with_card_theme_formats="fixed" ;;
           esac
           ;;
  all) with_card_theme_formats="svg,kde,fixed,sliced,pysol" ;;
esac

enable_card_theme_format_svg=no
enable_card_theme_format_kde=no
enable_card_theme_format_fixed=no
enable_card_theme_format_sliced=no
enable_card_theme_format_pysol=no

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
  require_cairo_1_10=yes
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
  [AS_HELP_STRING([--with-default-card-theme=NAME],[Which card theme to use by default (default: bonded)])],
  [],[case "$with_platform" in
        gnome|gtk-only) with_default_card_theme=bonded.svg ;;
        hildon) with_default_card_theme=paris.card-theme ;;
      esac])
AC_MSG_RESULT([$with_default_card_theme])

AC_DEFINE_UNQUOTED([AR_CARD_THEME_DEFAULT],["$with_default_card_theme"],[The default card theme])

AC_MSG_CHECKING([which card theme format to use by default])
AC_ARG_WITH([default-card-theme-format],
  [AS_HELP_STRING([--with-default-card-theme-format=NAME],[Which card theme format to use by default (default: platform dependent)])],
  [],[case "$with_platform" in
        gnome|gtk-only) with_default_card_theme_format=svg ;;
        hildon) with_default_card_theme_format=fixed ;;
      esac])
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
  2.0) if test "$with_platform" = "hildon" -a "$with_platform_variant" = "maemo3"; then
         GTK_REQUIRED=2.6.0
       elif test "$require_gtk_2_16" = "yes"; then
         GTK_REQUIRED=2.16.0
       else
         GTK_REQUIRED=2.8.0
       fi
       RSVG_REQUIRED=2.14.0
       LIBCANBERRA_GTK_REQUIRED=0
       LIBCANBERRA_GTK_PKGS="libcanberra-gtk >= $LIBCANBERRA_GTK_REQUIRED"
        ;;
  3.0) GTK_REQUIRED=2.91.7
       RSVG_REQUIRED=2.32.0
       LIBCANBERRA_GTK_REQUIRED=0.26
       LIBCANBERRA_GTK_PKGS="libcanberra-gtk3 >= $LIBCANBERRA_GTK_REQUIRED"
       ;;
esac

GCONF_REQUIRED=2.0
GIO_REQUIRED=2.25.7
GSTREAMER_REQUIRED=0.10.11
HILDON_LIBS_REQUIRED=0.14
HILDON_1_REQUIRED=1.00

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

# GDBus

PKG_CHECK_MODULES([GIO],[gio-2.0 >= $GIO_REQUIRED],[have_gio_2_26=yes],[have_gio_2_26=no])

# GSettings

AM_CONDITIONAL([HAVE_GIO_2_26],[test "$have_gio_2_26" = "yes"])

m4_ifdef([GLIB_GSETTINGS],[GLIB_GSETTINGS([],[:])])

# Check for GNOME modules

AM_GCONF_SOURCE_2
AC_PATH_PROG([GCONFTOOL],[gconftool-2])

if test "$with_platform" = "gnome"; then
  PKG_CHECK_MODULES([GNOME],[gconf-2.0 >= $GCONF_REQUIRED])
  AC_SUBST([GNOME_CFLAGS])
  AC_SUBST([GNOME_LIBS])

  need_gthread=yes
fi

# Check for Hildon modules

if test "$with_platform" = "hildon"; then
  if test "$with_platform_variant" = "maemo3"; then
    PKG_CHECK_MODULES([HILDON],[
      libosso
      hildon-libs >= $HILDON_LIBS_REQUIRED])
  else
    PKG_CHECK_MODULES([HILDON],[
      libosso
      dbus-1
      hildon-1 >= $HILDON_1_REQUIRED])
  fi

  AC_SUBST([HILDON_CFLAGS])
  AC_SUBST([HILDON_LIBS])
fi

# Check for librsvg

have_rsvg=no
if test "$need_rsvg" = "yes"; then
  have_rsvg=yes

  if test "$require_cairo_1_10" = "yes"; then
    CAIRO_REQUIRED=1.10.0
  else
    CAIRO_REQUIRED=1.0
  fi

  # Errors out if rsvg is not found
  PKG_CHECK_MODULES([RSVG],[
    librsvg-2.0 >= $RSVG_REQUIRED
    cairo >= $CAIRO_REQUIRED])

  AC_SUBST([RSVG_CFLAGS])
  AC_SUBST([RSVG_LIBS])

  AC_DEFINE([HAVE_RSVG],[1],[Refine if librsvg is available])

  # This only existed in some pre-gio rsvg versions
  if test "$with_gtk" = "2.0"; then
    rsvg_gnomevfs=$($PKG_CONFIG --variable=gnome_vfs_supported librsvg-2.0)
    if test "$rsvg_gnomevfs" = "yes"; then
      AC_DEFINE([HAVE_RSVG_GNOMEVFS],[1],[Define if librsvg has gnome-vfs support])

      need_gthread=yes
    fi
  fi
fi

AM_CONDITIONAL([HAVE_RSVG],[test "$have_rsvg" = "yes"])

# Check for Clutter

if test "$need_clutter" = "yes"; then
  CLUTTER_REQUIRED=1.0.0

  PKG_CHECK_MODULES([CLUTTER],[clutter-1.0 >= $CLUTTER_REQUIRED])
  AC_SUBST([CLUTTER_CFLAGS])
  AC_SUBST([CLUTTER_LIBS])

  case "$with_gtk" in
    2.0) CLUTTER_GTK_API_VERSION=0.10
         CLUTTER_GTK_REQUIRED=0.10.2
         ;;
    3.0) CLUTTER_GTK_API_VERSION=1.0
         CLUTTER_GTK_REQUIRED=0.91.6
         ;;
  esac

  PKG_CHECK_MODULES([CLUTTER_GTK],[clutter-gtk-$CLUTTER_GTK_API_VERSION >= $CLUTTER_GTK_REQUIRED])
  AC_SUBST([CLUTTER_GTK_CFLAGS])
  AC_SUBST([CLUTTER_GTK_LIBS])
fi

AM_CONDITIONAL([HAVE_CLUTTER],[test "$need_clutter" = "yes"])

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
if test "$allow_smclient" = "yes"; then
  AC_MSG_CHECKING([for GDK target])
  case "$with_gtk" in
    2.0) GDK_TARGET="$($PKG_CONFIG --variable target gdk-$GTK_API_VERSION)" ;;
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
    [case "$with_platform" in
       gnome|gtk-only) case "$GDK_TARGET" in
                         x11) with_smclient=xsmp SMCLIENT_PKGS="sm ice" ;;
                         win32|quartz) with_smclient=$GDK_TARGET ;;
                         *) with_smclient=no ;;
                       esac
                       ;;
       hildon) with_smclient=no ;;
     esac])
  AC_MSG_RESULT([$with_smclient])

  if test "$with_smclient" != "no"; then
    if test -n "$SMCLIENT_PKGS"; then
      PKG_CHECK_MODULES([SMCLIENT],[$SMCLIENT_PKGS])
    fi

    AC_DEFINE([WITH_SMCLIENT],[1],[Define if smclient is enabled])
  fi

else
  with_smclient=no
fi

AM_CONDITIONAL([WITH_SMCLIENT],[test "$with_smclient" != "no"])
AM_CONDITIONAL([WITH_SMCLIENT_XSMP],[test "$with_smclient" = "xsmp"])
AM_CONDITIONAL([WITH_SMCLIENT_WIN32],[test "$with_smclient" = "win32"])
AM_CONDITIONAL([WITH_SMCLIENT_QUARTZ],[test "$with_smclient" = "quartz"])

# *****************
# Extra build tools
# *****************

AC_PATH_PROG([GLIB_GENMARSHAL],[glib-genmarshal])

# glib-genmarshal --internal only exists since 2.14
if $PKG_CONFIG --exists "glib-2.0 >= 2.14.0" ; then
  GLIB_GENMARSHAL_INTERNAL="--internal"
else
  GLIB_GENMARSHAL_INTERNAL=
fi
AC_SUBST([GLIB_GENMARSHAL_INTERNAL])

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

# ********************
# Game specific checks
# ********************

have_guile=no
have_guile_1_8=no
if test "$need_guile" = "yes"; then
  AC_ARG_VAR([GUILE],[path to guile])
  if test "$ac_cv_env_GUILE_set" != "set"; then
    AC_PATH_TOOL([GUILE],[guile])
  fi

  if test -z "$GUILE" ; then
    AC_MSG_ERROR([guile not found; install guile development headers or use --enable-omitgames=aisleriot])
  fi

  AC_ARG_VAR([GUILE_CONFIG],[path to guile-config])
  if test "$ac_cv_env_GUILE_CONFIG_set" != "set"; then
    AC_PATH_TOOL([GUILE_CONFIG],[guile-config])
  fi
  if test -z "$GUILE_CONFIG"; then
    AC_MSG_ERROR([guile not found; install guile development headers or use --enable-omitgames=aisleriot])
  fi

  GUILE_CFLAGS=`$GUILE_CONFIG compile`
  GUILE_LIBS=`$GUILE_CONFIG link`
  AC_SUBST([GUILE_CFLAGS])
  AC_SUBST([GUILE_LIBS])

  # Checking for the header file is basically the only way to find
  # out if the _development_ files are present. guile-config returns
  # a value regardless of what is actually installed. The problem
  # with this is if the header ever gets moved to a non-default location.
  gg_save_CFLAGS="$CFLAGS"
  CFLAGS="$GUILE_CFLAGS $CFLAGS"
  AC_CHECK_HEADER([libguile.h],[have_guile=yes],
    [AC_MSG_ERROR([guile not found; install guile development headers or use --enable-omitgames=aisleriot])])
  CFLAGS="$gg_save_CFLAGS"

  # On 64-bit machines guile < 1.6.5 causes a crash in aisleriot
  if uname -m | grep -q 64 > /dev/null; then
    GUILE_TARGET="1.6.5"
  else
    GUILE_TARGET="1.6.0"
  fi
  if test `$GUILE -c '(if (string<? (version) (cadr (command-line))) (display "no") (display "yes")) (newline)' $GUILE_TARGET` != yes ; then
    AC_MSG_ERROR([Your version of guile is too old, please install a recent guile package (>= $GUILE_TARGET)])
  fi

  have_guile_1_8=$($GUILE -c '(if (string<? (version) (cadr (command-line))) (display "no") (display "yes")) (newline)' 1.8.0)

  if test "$have_guile_1_8" = "yes"; then
    AC_DEFINE([HAVE_GUILE_1_8],[1],[Define if guile version is 1.8.0 or above])
  fi
fi

AM_CONDITIONAL([HAVE_GUILE],[test "$have_guile" = "yes"])
AM_CONDITIONAL([HAVE_GUILE_1_8],[test "$have_guile_1_8" = "yes"])

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
  [case "$with_platform" in
     hildon) with_help_method=file ;;
     *) if test "$platform_win32" = "yes"; then
           with_help_method=file
         else
           with_help_method=ghelp
         fi ;;
   esac])
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
data/Makefile
games/Makefile
help/Makefile
po/Makefile.in
sounds/Makefile
src/Makefile
src/icons/Makefile
src/icons/gnome/Makefile
src/icons/hildon/Makefile
src/lib/Makefile
src/smclient/Makefile
src/sol.desktop.in
src/freecell.desktop.in
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
