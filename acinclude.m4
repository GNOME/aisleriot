# Check for the presence of C99 features.  Generally the check will fail
# if the feature isn't present (a C99 compiler isn't that much to ask,
# right?).

# Check C99-style variadic macros (required):
#
#  #define PRINTF(msg, ...) (printf(msg, __VA_ARGS__)
#
AC_DEFUN([AC_C99_VARIADIC_MACROS],
[
  dnl Check for variadic macros
  AC_CACHE_CHECK([for C99 variadic macros],
    [ac_cv_c99_variadic_macros],
     [AC_TRY_COMPILE(
          [#include <stdio.h>
           #define MSG(...) fprintf(stderr, __VA_ARGS__)
          ],
          [MSG("foo");
           MSG("%s", "foo");
           MSG("%s%d", "foo", 1);],
          ac_cv_c99_variadic_macros=yes,
          ac_cv_c99_variadic_macros=no)])
  if test "x${ac_cv_c99_variadic_macros}" != "xyes"; then
    AC_MSG_ERROR([A compiler supporting C99 variadic macros is required])
  fi
])

# Check C99-style variable-sized arrays (required):
#
#   char concat_str[strlen(s1) + strlen(s2) + 1];
#
AC_DEFUN([AC_C99_VARIABLE_ARRAYS],
[
  dnl Check for variable arrays
  AC_CACHE_CHECK([for C99 variable arrays],
    [ac_cv_c99_variable_arrays],
    [AC_TRY_COMPILE(
        [],
        [char *s1 = "foo", *s2 = "bar";
         char s3[strlen(s1) + strlen(s2) + 1];
         sprintf(s3, "%s%s", s1, s2);],
        ac_cv_c99_variable_arrays=yes,
        ac_cv_c99_variable_arrays=no)])
  if test "x${ac_cv_c99_variable_arrays}" != "xyes"; then
    AC_MSG_ERROR([A compiler supporting C99 variable arrays is required])
  fi
])

# Check C99-style initializers (required):
#
# Examples:
#   struct timeval tv = {.tv_sec = 0, .tv_usec = 500000};
#   int fibonacci[6] = {[0] = 0, [1] = 1, [2] = 1, [3] = 2, [4] = 3, [5] = 5};
# Note we do not check for multi-field initializers like
#   struct { struct { int b; } a; } = {.a.b = 5}
# which are not supported by many compilers.  It is best to avoid this
# problem by writing these using nesting.  The above case becomes
#   struct { struct { int b; } a; } = {.a = {.b = 5}}
AC_DEFUN([AC_C99_INITIALIZERS],
[
  dnl Check for C99 initializers
  AC_CACHE_CHECK([for C99 initializers],
    [ac_cv_c99_initializers],
    [AC_TRY_COMPILE(
        [struct foo {
           int an_integer;
           char *a_string;
           int an_array[5];
           union {int x, y;} a_union;
         };
        ],
        [struct foo bar = {.an_array = {0, [3] = 2, [2] = 1, [4] = 3},
                           .an_integer = 999,
                           .a_string = "does it work?",
                           .a_union = {.y = 243}};],
        [ac_cv_c99_initializers=yes],
        [ac_cv_c99_initializers=no])])
  if test "${ac_cv_c99_initializers}" != "yes"; then
    AC_MSG_ERROR([A compiler supporting C99 initializers is required])
  fi
])

# Check C99-style stdint.h (required)
AC_DEFUN([AC_C99_STDINT_H],
[
  AC_CHECK_HEADERS([stdint.h])
  dnl Check for C99 stdint.h
  AC_CACHE_CHECK([for C99 stdint.h],
    [ac_cv_c99_stdint_h],
    [ac_cv_c99_stdint_h=$ac_cv_header_stdint_h])
  if test "${ac_cv_c99_stdint_h}" != "yes"; then
    AC_MSG_ERROR([A compiler supporting C99's stdint.h is required])
  fi
])
dnl ======================================
dnl GGZ Gaming Zone - Configuration Macros
dnl ======================================
dnl
dnl Copyright (C) 2001 - 2004 Josef Spillner, josef@ggzgamingzone.org
dnl This file has heavily been inspired by KDE's acinclude :)
dnl It is published under the conditions of the GNU General Public License.
dnl
dnl ======================================
dnl
dnl This file is common to most GGZ modules, and should be kept in sync
dnl between them all.  The master copy resides with libggz.
dnl Currently the following modules use it:
dnl   kde-games, kde-client, gtk-games, gtk-client, utils, grubby,
dnl   ggz-client-libs, ggzd, gnome-client, txt-client, sdl-games, libggz
dnl See /docs/ggz-project/buildsystem for documentation.
dnl
dnl ======================================
dnl
dnl History:
dnl   See the CVS log for a full history.
dnl
dnl ------------------------------------------------------------------------
dnl Content of this file:
dnl ------------------------------------------------------------------------
dnl High-level macros:
dnl   AC_GGZ_CHECK - Checks for presence of GGZ client and server libraries.
dnl                  GGZ users can call this macro to determine at compile
dnl                  time whether to include GGZ support.  Server and client
dnl                  are checked separately.  GGZ_SERVER and GGZ_CLIENT are
dnl                  defined in config.h, and created as conditionals in
dnl                  the Makefiles.
dnl
dnl Low-level macros:
dnl   AC_GGZ_INIT - initialization and paths/options setup
dnl   AC_GGZ_VERSION - ensure a minimum version of GGZ
dnl   AC_GGZ_LIBGGZ - find the libggz headers and libraries
dnl   AC_GGZ_GGZCORE - find the ggzcore headers and libraries
dnl   AC_GGZ_CONFIG - find the ggz-config tool and set up configuration
dnl   AC_GGZ_GGZMOD - find the ggzmod library
dnl   AC_GGZ_GGZDMOD - find the ggzdmod library
dnl   AC_GGZ_SERVER - set up game and room path for ggzd game servers
dnl
dnl   Each macro takes two arguments:
dnl     1.  Action-if-found (or empty for no action).
dnl     2.  Action-if-not-found (or empty for error, or "ignore" to ignore).
dnl
dnl Internal functions:
dnl   AC_GGZ_ERROR - user-friendly error messages
dnl   AC_GGZ_FIND_FILE - macro for convenience (thanks kde)
dnl   AC_GGZ_REMOVEDUPS - eliminate duplicate list elements
dnl

dnl ------------------------------------------------------------------------
dnl Find a directory containing a single file
dnl Synopsis: AC_GGZ_FIND_FILE(file, directorylist, <returnvar>)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_FIND_FILE],
[
$3=NO
for i in $2;
do
  for j in $1;
  do
    echo "configure: __oline__: $i/$j" >&AC_FD_CC
    if test -r "$i/$j"; then
      echo "taking that" >&AC_FD_CC
      $3=$i
      break 2
    fi
  done
done
])

dnl ------------------------------------------------------------------------
dnl Remove duplicate entries in a list, and remove all NO's
dnl Synopsis: AC_GGZ_REMOVEDUPS(list, <returnlist>)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_REMOVEDUPS],
[
ret=""
for i in $1; do
  add=yes
  for j in $ret; do
    if test "x$i" = "x$j"; then
      add=no
    fi
  done
  if test "x$i" = "xNO"; then
    add=no
  fi
  if test "x$add" = "xyes"; then
  ret="$ret $i"
  fi
done
$2=$ret
])

dnl ------------------------------------------------------------------------
dnl User-friendly error messages
dnl Synopsis: AC_GGZ_ERROR(libraryname, headerdirlist, libdirlist)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_ERROR],
[
  AC_MSG_WARN([no
  The library '$1' does not seem to be installed correctly.
  Headers searched in: $2
  Libraries searched in: $3
  Please read QuickStart.GGZ in order to fix this.
  ])
  exit 1
])

dnl ------------------------------------------------------------------------
dnl Initialization, common values and such
dnl Synopsis: AC_GGZ_INIT([export], [defaults])
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_INIT],
[
if test "x$prefix" = "xNONE"; then
  prefix="${ac_default_prefix}"
fi
if test "x$exec_prefix" = "xNONE"; then
  exec_prefix='${prefix}'
fi
AC_DEFINE_UNQUOTED([PREFIX], "${prefix}", [The installation prefix])

ac_ggz_prefix=""
AC_ARG_WITH(ggz-dir,
    AC_HELP_STRING([--with-ggz-dir=DIR], [Path to GGZ Gaming Zone]),
    [  ac_ggz_prefix="$withval"
    ])

if test "x${prefix}" = "xNONE"; then
   ac_ggz_prefix_incdir="${ac_default_prefix}/include"
   ac_ggz_prefix_libdir="${ac_default_prefix}/lib"
   ac_ggz_prefix_bindir="${ac_default_prefix}/bin"
   ac_ggz_prefix_etcdir="${ac_default_prefix}/etc"
else
   unq_includedir="${includedir}"
   unq_libdir="${libdir}"
   unq_bindir="${bindir}"
   unq_sysconfdir="${sysconfdir}"

   eval unq_includedir=`echo $unq_includedir`
   eval unq_includedir=`echo $unq_includedir`
   eval unq_libdir=`echo $unq_libdir`
   eval unq_libdir=`echo $unq_libdir`
   eval unq_bindir=`echo $unq_bindir`
   eval unq_bindir=`echo $unq_bindir`
   eval unq_sysconfdir=`echo $unq_sysconfdir`
   eval unq_sysconfdir=`echo $unq_sysconfdir`

   ac_ggz_prefix_incdir="${unq_includedir}"
   ac_ggz_prefix_libdir="${unq_libdir}"
   ac_ggz_prefix_bindir="${unq_bindir}"
   ac_ggz_prefix_etcdir="${unq_sysconfdir}"
fi
ac_ggz_stdinc="$ac_ggz_prefix_incdir"
ac_ggz_stdlib="$ac_ggz_prefix_libdir"
ac_ggz_stdbin="$ac_ggz_prefix_bindir"
ac_ggz_stdetc="$ac_ggz_prefix_etcdir/ggzd"
if test "x$ac_ggz_prefix" != "x"; then
  ac_ggz_stdinc="$ac_ggz_stdinc $ac_ggz_prefix/include"
  ac_ggz_stdlib="$ac_ggz_stdlib $ac_ggz_prefix/lib $ac_ggz_prefix/lib64"
  ac_ggz_stdbin="$ac_ggz_stdbin $ac_ggz_prefix/bin"
  ac_ggz_stdetc="$ac_ggz_stdetc $ac_ggz_prefix/etc/ggzd"
fi
if test "x$1" = "xdefaults" || test "x$2" = "xdefaults"; then
  ac_ggz_stdinc="$ac_ggz_stdinc /usr/local/include /usr/include"
  ac_ggz_stdlib="$ac_ggz_stdlib /usr/local/lib /usr/local/lib64 /usr/lib /usr/lib64"
  ac_ggz_stdbin="$ac_ggz_stdbin /usr/local/bin /usr/bin"
  ac_ggz_stdetc="$ac_ggz_stdetc /usr/local/etc/ggzd /etc/ggzd"
fi
if test "x$1" = "xexport" || test "x$2" = "xexport"; then
  CPPFLAGS="$CPPFLAGS -isystem ${ac_ggz_prefix_incdir} -isystem /usr/local/include"
  LDFLAGS="$LDFLAGS -L${ac_ggz_prefix_libdir} -L/usr/local/lib"
fi

save_cflags=$CFLAGS
save_cxxflags=$CXXFLAGS
CFLAGS="-Wall -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
	[[void signedness(void){char c;if(c==-1)c=0;}]])],
	[],
	[save_cflags="$save_cflags -fsigned-char"
	 save_cxxflags="$save_cxxflags -fsigned-char"])
CFLAGS=$save_cflags
CXXFLAGS=$save_cxxflags
])

dnl ------------------------------------------------------------------------
dnl Ensure that a minimum version of GGZ is present
dnl Synopsis: AC_GGZ_VERSION(major, minor, micro,
dnl                          action-if-found, action-if-not-found)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_VERSION],
[
	major=$1
	minor=$2
	micro=$3

	testprologue="#include <ggz.h>"
	testbody=""
	testbody="$testbody if(LIBGGZ_VERSION_MAJOR > $major) return 0;"
	testbody="$testbody if(LIBGGZ_VERSION_MAJOR < $major) return -1;"
	testbody="$testbody if(LIBGGZ_VERSION_MINOR > $minor) return 0;"
	testbody="$testbody if(LIBGGZ_VERSION_MINOR < $minor) return -1;"
	testbody="$testbody if(LIBGGZ_VERSION_MICRO > $micro) return 0;"
	testbody="$testbody if(LIBGGZ_VERSION_MICRO < $micro) return -1;"
	testbody="$testbody return 0;"

	AC_MSG_CHECKING([for GGZ library version: $major.$minor.$micro])
	AC_RUN_IFELSE(
		[AC_LANG_PROGRAM([[$testprologue]], [[$testbody]])],
		[ac_ggz_version_check=yes],
		[ac_ggz_version_check=no]
	)
	if test "x$ac_ggz_version_check" = "xyes"; then
		AC_MSG_RESULT([yes])
		$4
	else
		AC_MSG_RESULT([no])
		if test "x$5" = "x"; then
			AC_MSG_ERROR([The GGZ version is too old. Version $major.$minor.$micro is required.])
		fi
		$5
	fi
])

dnl ------------------------------------------------------------------------
dnl Try to find the libggz headers and libraries.
dnl $(LIBGGZ_LDFLAGS) will be -L ... (if needed)
dnl and $(LIBGGZ_INCLUDES) will be -I ... (if needed)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_LIBGGZ],
[
AC_MSG_CHECKING([for GGZ library: libggz])

ac_libggz_includes=NO ac_libggz_libraries=NO
libggz_libraries=""
libggz_includes=""

AC_ARG_WITH(libggz-dir,
    AC_HELP_STRING([--with-libggz-dir=DIR],[libggz installation prefix]),
    [  ac_libggz_includes="$withval"/include
       ac_libggz_libraries="$withval"/lib
    ])
AC_ARG_WITH(libggz-includes,
    AC_HELP_STRING([--with-libggz-includes=DIR],
                   [where the libggz includes are]),
    [  ac_libggz_includes="$withval"
    ])
AC_ARG_WITH(libggz-libraries,
    AC_HELP_STRING([--with-libggz-libraries=DIR],[where the libggz libs are]),
    [  ac_libggz_libraries="$withval"
    ])

AC_CACHE_VAL(ac_cv_have_libggz,
[
libggz_incdirs="$ac_libggz_includes $ac_ggz_stdinc"
AC_GGZ_REMOVEDUPS($libggz_incdirs, libggz_incdirs)
libggz_header=ggz.h

AC_GGZ_FIND_FILE($libggz_header, $libggz_incdirs, libggz_incdir)
ac_libggz_includes="$libggz_incdir"

libggz_libdirs="$ac_libggz_libraries $ac_ggz_stdlib"
AC_GGZ_REMOVEDUPS($libggz_libdirs, libggz_libdirs)

libggz_libdir=NO
for dir in $libggz_libdirs; do
  try="ls -1 $dir/libggz.la $dir/libggz.so"
  if test -n "`$try 2> /dev/null`"; then libggz_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
done

ac_libggz_libraries="$libggz_libdir"

if test "$ac_libggz_includes" = NO || test "$ac_libggz_libraries" = NO; then
  ac_cv_have_libggz="have_libggz=no"
  ac_libggz_notfound=""
else
  have_libggz="yes"
fi
])

eval "$ac_cv_have_libggz"

if test "$have_libggz" != yes; then
  if test "x$2" = "xignore"; then
    AC_MSG_RESULT([$have_libggz (ignored)])
  else
    AC_MSG_RESULT([$have_libggz])
    if test "x$2" = "x"; then
      AC_GGZ_ERROR(libggz, $libggz_incdirs, $libggz_libdirs)
    fi

    # perform actions given by argument 2.
    $2
  fi
else
  ac_cv_have_libggz="have_libggz=yes \
    ac_libggz_includes=$ac_libggz_includes ac_libggz_libraries=$ac_libggz_libraries"
  AC_MSG_RESULT([$have_libggz (libraries $ac_libggz_libraries, headers $ac_libggz_includes)])

  libggz_libraries="$ac_libggz_libraries"
  libggz_includes="$ac_libggz_includes"

  AC_SUBST(libggz_libraries)
  AC_SUBST(libggz_includes)

  LIBGGZ_INCLUDES="-isystem $libggz_includes"
  LIBGGZ_LDFLAGS="-L$libggz_libraries"

  AC_SUBST(LIBGGZ_INCLUDES)
  AC_SUBST(LIBGGZ_LDFLAGS)

  LIB_GGZ='-lggz'
  AC_SUBST(LIB_GGZ)

  # perform actions given by argument 1.
  $1
fi

])


dnl ------------------------------------------------------------------------
dnl Try to find the ggzcore headers and libraries.
dnl $(GGZCORE_LDFLAGS) will be -L ... (if needed)
dnl and $(GGZCORE_INCLUDES) will be -I ... (if needed)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_GGZCORE],
[
AC_MSG_CHECKING([for GGZ library: ggzcore])

ac_ggzcore_includes=NO ac_ggzcore_libraries=NO
ggzcore_libraries=""
ggzcore_includes=""

AC_ARG_WITH(ggzcore-dir,
    AC_HELP_STRING([--with-ggzcore-dir=DIR],[ggzcore installation prefix]),
    [  ac_ggzcore_includes="$withval"/include
       ac_ggzcore_libraries="$withval"/lib
    ])
AC_ARG_WITH(ggzcore-includes,
    AC_HELP_STRING([--with-ggzcore-includes=DIR],
                   [where the ggzcore includes are]),
    [  ac_ggzcore_includes="$withval"
    ])
AC_ARG_WITH(ggzcore-libraries,
    AC_HELP_STRING([--with-ggzcore-libraries=DIR],
                   [where the ggzcore libs are]),
    [  ac_ggzcore_libraries="$withval"
    ])

AC_CACHE_VAL(ac_cv_have_ggzcore,
[
ggzcore_incdirs="$ac_ggzcore_includes $ac_ggz_stdinc"
AC_GGZ_REMOVEDUPS($ggzcore_incdirs, ggzcore_incdirs)
ggzcore_header=ggzcore.h

AC_GGZ_FIND_FILE($ggzcore_header, $ggzcore_incdirs, ggzcore_incdir)
ac_ggzcore_includes="$ggzcore_incdir"

ggzcore_libdirs="$ac_ggzcore_libraries $ac_ggz_stdlib"
AC_GGZ_REMOVEDUPS($ggzcore_libdirs, ggzcore_libdirs)

ggzcore_libdir=NO
for dir in $ggzcore_libdirs; do
  try="ls -1 $dir/libggzcore.la $dir/libggzcore.so"
  if test -n "`$try 2> /dev/null`"; then ggzcore_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
done

ac_ggzcore_libraries="$ggzcore_libdir"

if test "$ac_ggzcore_includes" = NO || test "$ac_ggzcore_libraries" = NO; then
  ac_cv_have_ggzcore="have_ggzcore=no"
  ac_ggzcore_notfound=""
else
  have_ggzcore="yes"
fi
])

eval "$ac_cv_have_ggzcore"

if test "$have_ggzcore" != yes; then
  if test "x$2" = "xignore"; then
    AC_MSG_RESULT([$have_ggzcore (intentionally ignored)])
  else
    AC_MSG_RESULT([$have_ggzcore])
    if test "x$2" = "x"; then
      AC_GGZ_ERROR(ggzcore, $ggzcore_incdirs, $ggzcore_libdirs)
    fi

    # Perform actions given by argument 2.
    $2
  fi
else
  ac_cv_have_ggzcore="have_ggzcore=yes \
    ac_ggzcore_includes=$ac_ggzcore_includes ac_ggzcore_libraries=$ac_ggzcore_libraries"
  AC_MSG_RESULT([$have_ggzcore (libraries $ac_ggzcore_libraries, headers $ac_ggzcore_includes)])

  ggzcore_libraries="$ac_ggzcore_libraries"
  ggzcore_includes="$ac_ggzcore_includes"

  AC_SUBST(ggzcore_libraries)
  AC_SUBST(ggzcore_includes)

  GGZCORE_INCLUDES="-isystem $ggzcore_includes"
  GGZCORE_LDFLAGS="-L$ggzcore_libraries"

  AC_SUBST(GGZCORE_INCLUDES)
  AC_SUBST(GGZCORE_LDFLAGS)

  LIB_GGZCORE='-lggzcore'
  AC_SUBST(LIB_GGZCORE)

  # Perform actions given by argument 1.
  $1
fi

])

dnl ------------------------------------------------------------------------
dnl Try to find the ggz-config binary.
dnl Sets GGZ_CONFIG to the path/name of the program.
dnl Sets also: ggz_gamedir, ggz_datadir etc.
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_CONFIG],
[
AC_MSG_CHECKING([for GGZ configuration tool: ggz-config])

ac_ggz_config=NO
ggz_config=""

AC_ARG_WITH(ggzconfig,
    AC_HELP_STRING([--with-ggzconfig=DIR],[path to ggz-config]),
    [  ac_ggz_config="$withval"
    ])

AC_CACHE_VAL(ac_cv_have_ggzconfig,
[
ggz_config_dirs="$ac_ggz_config $ac_ggz_stdbin"

AC_GGZ_FIND_FILE(ggz-config, $ggz_config_dirs, ggz_config_dir)
ac_ggz_config="$ggz_config_dir"

if test "$ac_ggz_config" = NO; then
  ac_cv_have_ggzcore="have_ggz_config=no"
  ac_ggz_config_notfound=""
  have_ggz_config="no"
else
  have_ggz_config="yes"
fi
])

eval "$ac_cv_have_ggz_config"

if test "$have_ggz_config" != yes; then
  if test "x$2" = "xignore"; then
    AC_MSG_RESULT([$have_ggz_config (intentionally ignored)])
    GGZ_CONFIG="true"
    ggzexecmoddir="\${prefix}/lib/ggz"
    ggzdatadir="\${prefix}/share/ggz"
    AC_SUBST(GGZ_CONFIG)
    AC_SUBST(ggzexecmoddir)
    AC_SUBST(ggzdatadir)
    AC_DEFINE_UNQUOTED(GAMEDIR, "${prefix}/lib/ggz", [Path where to install the games])
    AC_DEFINE_UNQUOTED(GGZDATADIR, "${prefix}/share/ggz", [Path where the games should look for their data files])
  else
    AC_MSG_RESULT([$have_ggz_config])
    if test "x$2" = "x"; then
      AC_MSG_ERROR([ggz-config not found. Please check your installation! ])
    fi

    # Perform actions given by argument 2.
    $2
  fi
else
  pathto_app=`echo $prefix/bin/ | tr -s "/"`
  pathto_ggz=`echo $ac_ggz_config/ | tr -s "/"`

  if test "x$pathto_app" != "x$pathto_ggz"; then
    AC_MSG_RESULT([$have_ggz_config (dismissed due to different prefix)])
    GGZ_CONFIG="true"
    ggzexecmoddir="\${prefix}/lib/ggz"
    ggzdatadir="\${prefix}/share/ggz"
    AC_SUBST(GGZ_CONFIG)
    AC_SUBST(ggzexecmoddir)
    AC_SUBST(ggzdatadir)
    AC_DEFINE_UNQUOTED(GGZMODULECONFDIR, "${prefix}/etc", [Path where the game registry is located])
    AC_DEFINE_UNQUOTED(GAMEDIR, "${prefix}/lib/ggz", [Path where to install the games])
    AC_DEFINE_UNQUOTED(GGZDATADIR, "${prefix}/share/ggz", [Path where the games should look for their data files])
  else
    ac_cv_have_ggz_config="have_ggz_config=yes \
      ac_ggz_config=$ac_ggz_config"
    AC_MSG_RESULT([$ac_ggz_config/ggz-config])

    ggz_config="$ac_ggz_config"
    AC_SUBST(ggz_config)

    AC_ARG_ENABLE([noregistry],
      AC_HELP_STRING([--enable-noregistry], [Do not register game modules.]),
      [enable_noregistry=yes], [enable_noregistry=no])

    GGZ_CONFIG="${ggz_config}/ggz-config"
    if test "$enable_noregistry" = yes; then
      GGZ_CONFIG="$GGZ_CONFIG --noregistry=$enableval"
    fi
    AC_SUBST(GGZ_CONFIG)

    ggzmoduleconfdir=`$GGZ_CONFIG --configdir`
    AC_DEFINE_UNQUOTED(GGZMODULECONFDIR, "${ggzmoduleconfdir}", [Path where the game registry is located])
    ggzexecmoddir=`$GGZ_CONFIG --gamedir`
    AC_DEFINE_UNQUOTED(GAMEDIR, "${ggzexecmoddir}", [Path where to install the games])
    ggzdatadir=`$GGZ_CONFIG --datadir`
    AC_DEFINE_UNQUOTED(GGZDATADIR, "${ggzdatadir}", [Path where the games should look for their data files])
    packagesrcdir=`cd $srcdir && pwd`
    AC_DEFINE_UNQUOTED(PACKAGE_SOURCE_DIR, "${packagesrcdir}", [Path where the source is located])

    AC_SUBST(ggzmoduleconfdir)
    AC_SUBST(ggzexecmoddir)
    AC_SUBST(ggzdatadir)
    AC_SUBST(packagesrcdir)

    # Perform actions given by argument 1.
    $1
  fi
fi

])

dnl ------------------------------------------------------------------------
dnl Try to find the ggzmod headers and libraries.
dnl $(GGZMOD_LDFLAGS) will be -L ... (if needed)
dnl and $(GGZMOD_INCLUDES) will be -I ... (if needed)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_GGZMOD],
[
AC_MSG_CHECKING([for GGZ library: ggzmod])

ac_ggzmod_includes=NO ac_ggzmod_libraries=NO
ggzmod_libraries=""
ggzmod_includes=""

AC_ARG_WITH(ggzmod-dir,
    AC_HELP_STRING([--with-ggzmod-dir=DIR],[ggzmod installation prefix]),
    [  ac_ggzmod_includes="$withval"/include
       ac_ggzmod_libraries="$withval"/lib
    ])
AC_ARG_WITH(ggzmod-includes,
    AC_HELP_STRING([--with-ggzmod-includes=DIR],
                   [where the ggzmod includes are]),
    [  ac_ggzmod_includes="$withval"
    ])
AC_ARG_WITH(ggzmod-libraries,
    AC_HELP_STRING([--with-ggzmod-libraries=DIR],
                   [where the ggzmod libs are]),
    [  ac_ggzmod_libraries="$withval"
    ])

AC_CACHE_VAL(ac_cv_have_ggzmod,
[
ggzmod_incdirs="$ac_ggzmod_includes $ac_ggz_stdinc"
AC_GGZ_REMOVEDUPS($ggzmod_incdirs, ggzmod_incdirs)
ggzmod_header=ggzmod.h

AC_GGZ_FIND_FILE($ggzmod_header, $ggzmod_incdirs, ggzmod_incdir)
ac_ggzmod_includes="$ggzmod_incdir"

ggzmod_libdirs="$ac_ggzmod_libraries $ac_ggz_stdlib"
AC_GGZ_REMOVEDUPS($ggzmod_libdirs, ggzmod_libdirs)

ggzmod_libdir=NO
for dir in $ggzmod_libdirs; do
  try="ls -1 $dir/libggzmod.la $dir/libggzmod.so"
  if test -n "`$try 2> /dev/null`"; then ggzmod_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
done

ac_ggzmod_libraries="$ggzmod_libdir"

if test "$ac_ggzmod_includes" = NO || test "$ac_ggzmod_libraries" = NO; then
  ac_cv_have_ggzmod="have_ggzmod=no"
  ac_ggzmod_notfound=""
else
  have_ggzmod="yes"
fi
])

eval "$ac_cv_have_ggzmod"

if test "$have_ggzmod" != yes; then
  if test "x$2" = "xignore"; then
    AC_MSG_RESULT([$have_ggzmod (intentionally ignored)])
  else
    AC_MSG_RESULT([$have_ggzmod])
    if test "x$2" = "x"; then
      AC_GGZ_ERROR(ggzmod, $ggzmod_incdirs, $ggzmod_libdirs)
    fi

    # Perform actions given by argument 2.
    $2
  fi
else
  ac_cv_have_ggzmod="have_ggzmod=yes \
    ac_ggzmod_includes=$ac_ggzmod_includes ac_ggzmod_libraries=$ac_ggzmod_libraries"
  AC_MSG_RESULT([$have_ggzmod (libraries $ac_ggzmod_libraries, headers $ac_ggzmod_includes)])

  ggzmod_libraries="$ac_ggzmod_libraries"
  ggzmod_includes="$ac_ggzmod_includes"

  AC_SUBST(ggzmod_libraries)
  AC_SUBST(ggzmod_includes)

  GGZMOD_INCLUDES="-isystem $ggzmod_includes"
  GGZMOD_LDFLAGS="-L$ggzmod_libraries"

  AC_SUBST(GGZMOD_INCLUDES)
  AC_SUBST(GGZMOD_LDFLAGS)

  LIB_GGZMOD='-lggzmod'
  AC_SUBST(LIB_GGZMOD)

  # Perform actions given by argument 1.
  $1
fi

])

dnl ------------------------------------------------------------------------
dnl Try to find the ggzdmod headers and libraries.
dnl $(GGZDMOD_LDFLAGS) will be -L ... (if needed)
dnl and $(GGZDMOD_INCLUDES) will be -I ... (if needed)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_GGZDMOD],
[
AC_MSG_CHECKING([for GGZ library: ggzdmod])

ac_ggzdmod_includes=NO ac_ggzdmod_libraries=NO
ggzdmod_libraries=""
ggzdmod_includes=""

AC_ARG_WITH(ggzdmod-dir,
    AC_HELP_STRING([--with-ggzdmod-dir=DIR], [ggzdmod installation prefix]),
    [  ac_ggzdmod_includes="$withval"/include
       ac_ggzdmod_libraries="$withval"/lib
    ])
AC_ARG_WITH(ggzdmod-includes,
    AC_HELP_STRING([--with-ggzdmod-includes=DIR], 
                   [where the ggzdmod includes are]),
    [  ac_ggzdmod_includes="$withval"
    ])
AC_ARG_WITH(ggzdmod-libraries,
    AC_HELP_STRING([--with-ggzdmod-libraries=DIR],
                   [where the ggzdmod libs are]),
    [  ac_ggzdmod_libraries="$withval"
    ])

AC_CACHE_VAL(ac_cv_have_ggzdmod,
[
ggzdmod_incdirs="$ac_ggzdmod_includes $ac_ggz_stdinc"
AC_GGZ_REMOVEDUPS($ggzdmod_incdirs, ggzdmod_incdirs)
ggzdmod_header=ggzdmod.h

AC_GGZ_FIND_FILE($ggzdmod_header, $ggzdmod_incdirs, ggzdmod_incdir)
ac_ggzdmod_includes="$ggzdmod_incdir"

ggzdmod_libdirs="$ac_ggzdmod_libraries $ac_ggz_stdlib"
AC_GGZ_REMOVEDUPS($ggzdmod_libdirs, ggzdmod_libdirs)

ggzdmod_libdir=NO
for dir in $ggzdmod_libdirs; do
  try="ls -1 $dir/libggzdmod.la $dir/libggzdmod.so"
  if test -n "`$try 2> /dev/null`"; then ggzdmod_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
done

ac_ggzdmod_libraries="$ggzdmod_libdir"

if test "$ac_ggzdmod_includes" = NO || test "$ac_ggzdmod_libraries" = NO; then
  ac_cv_have_ggzdmod="have_ggzdmod=no"
  ac_ggzdmod_notfound=""
else
  have_ggzdmod="yes"
fi
])

eval "$ac_cv_have_ggzdmod"

if test "$have_ggzdmod" != yes; then
  if test "x$2" = "xignore"; then
    AC_MSG_RESULT([$have_ggzdmod (intentionally ignored)])
  else
    AC_MSG_RESULT([$have_ggzdmod])
    if test "x$2" = "x"; then
      AC_GGZ_ERROR(ggzdmod, $ggzdmod_incdirs, $ggzdmod_libdirs)
    fi

    # Perform actions given by argument 2.
    $2
  fi
else
  ac_cv_have_ggzdmod="have_ggzdmod=yes \
    ac_ggzdmod_includes=$ac_ggzdmod_includes ac_ggzdmod_libraries=$ac_ggzdmod_libraries"
  AC_MSG_RESULT([$have_ggzdmod (libraries $ac_ggzdmod_libraries, headers $ac_ggzdmod_includes)])

  ggzdmod_libraries="$ac_ggzdmod_libraries"
  ggzdmod_includes="$ac_ggzdmod_includes"

  AC_SUBST(ggzdmod_libraries)
  AC_SUBST(ggzdmod_includes)

  GGZDMOD_INCLUDES="-isystem $ggzdmod_includes"
  GGZDMOD_LDFLAGS="-L$ggzdmod_libraries"

  AC_SUBST(GGZDMOD_INCLUDES)
  AC_SUBST(GGZDMOD_LDFLAGS)

  LIB_GGZDMOD='-lggzdmod'
  AC_SUBST(LIB_GGZDMOD)

  # Perform actions given by argument 1.
  $1
fi

])

dnl ------------------------------------------------------------------------
dnl Try to find the ggz-gtk headers and libraries.
dnl $(GGZGTK_LDFLAGS) will be -L ... (if needed)
dnl and $(GGZGTK_INCLUDES) will be -I ... (if needed)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_GTK],
[
AC_MSG_CHECKING([for GGZ library: ggz-gtk])

ac_ggz_gtk_includes=NO ac_ggz_gtk_libraries=NO
ggz_gtk_libraries=""
ggz_gtk_includes=""

AC_ARG_WITH(ggz-gtk-dir,
    AC_HELP_STRING([--with-ggz-gtk-dir=DIR], [ggz-gtk installation prefix]),
    [  ac_ggz_gtk_includes="$withval"/include
       ac_ggz_gtk_libraries="$withval"/lib
    ])
AC_ARG_WITH(ggz-gtk-includes,
    AC_HELP_STRING([--with-ggz-gtk-includes=DIR], 
                   [where the ggz-gtk includes are]),
    [  ac_ggz_gtk_includes="$withval"
    ])
AC_ARG_WITH(ggz-gtk-libraries,
    AC_HELP_STRING([--with-ggz-gtk-libraries=DIR],
                   [where the ggz-gtk libs are]),
    [  ac_ggz_gtk_libraries="$withval"
    ])

AC_CACHE_VAL(ac_cv_have_ggz_gtk,
[
ggz_gtk_incdirs="$ac_ggz_gtk_includes $ac_ggz_stdinc"
AC_GGZ_REMOVEDUPS($ggz_gtk_incdirs, ggz_gtk_incdirs)
ggz_gtk_header=ggz-gtk.h

AC_GGZ_FIND_FILE($ggz_gtk_header, $ggz_gtk_incdirs, ggz_gtk_incdir)
ac_ggz_gtk_includes="$ggz_gtk_incdir"

ggz_gtk_libdirs="$ac_ggz_gtk_libraries $ac_ggz_stdlib"
AC_GGZ_REMOVEDUPS($ggz_gtk_libdirs, ggz_gtk_libdirs)

ggz_gtk_libdir=NO
for dir in $ggz_gtk_libdirs; do
  try="ls -1 $dir/libggz-gtk.la $dir/libggz-gtk.so"
  if test -n "`$try 2> /dev/null`"; then ggz_gtk_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
done

ac_ggz_gtk_libraries="$ggz_gtk_libdir"

if test "$ac_ggz_gtk_includes" = NO || test "$ac_ggz_gtk_libraries" = NO; then
  ac_cv_have_ggz_gtk="have_ggz_gtk=no"
  ac_ggz_gtk_notfound=""
else
  have_ggz_gtk="yes"
fi
])

eval "$ac_cv_have_ggz_gtk"

if test "$have_ggz_gtk" != yes; then
  if test "x$2" = "xignore"; then
    AC_MSG_RESULT([$have_ggz_gtk (intentionally ignored)])
  else
    AC_MSG_RESULT([$have_ggz_gtk])
    if test "x$2" = "x"; then
      AC_GGZ_ERROR(ggz-gtk, $ggz_gtk_incdirs, $ggz_gtk_libdirs)
    fi

    # Perform actions given by argument 2.
    $2
  fi
else
  ac_cv_have_ggz_gtk="have_ggz_gtk=yes \
    ac_ggz_gtk_includes=$ac_ggz_gtk_includes ac_ggz_gtk_libraries=$ac_ggz_gtk_libraries"
  AC_MSG_RESULT([$have_ggz_gtk (libraries $ac_ggz_gtk_libraries, headers $ac_ggz_gtk_includes)])

  ggz_gtk_libraries="$ac_ggz_gtk_libraries"
  ggz_gtk_includes="$ac_ggz_gtk_includes"

  AC_SUBST(ggz_gtk_libraries)
  AC_SUBST(ggz_gtk_includes)

  GGZ_GTK_INCLUDES="-isystem $ggz_gtk_includes"
  GGZ_GTK_LDFLAGS="-L$ggz_gtk_libraries"

  AC_SUBST(GGZ_GTK_INCLUDES)
  AC_SUBST(GGZ_GTK_LDFLAGS)

  LIB_GGZ_GTK='-lggz-gtk'
  AC_SUBST(LIB_GGZ_GTK)

  # Perform actions given by argument 1.
  $1
fi
])

dnl ------------------------------------------------------------------------
dnl Setup the game server configuration.
dnl Sets ggzdconfdir (ggzd configuration).
dnl Sets ggzddatadir (for game server data).
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_SERVER],
[
AC_MSG_CHECKING([for GGZ server: ggzd])
AC_ARG_WITH(ggzd-confdir,
    AC_HELP_STRING([--with-ggzd-confdir=DIR], [directory for room/game data]),
[ ac_ggzd_confdir="$withval"
])

AC_CACHE_VAL(ac_cv_have_ggzdconf,
[
	if test "x$1" = "xforce"; then
		if test "x$ac_ggzd_confdir" = "x"; then
			ggzdconfdirs="$ac_ggz_stdetc"
		else
			ggzdconfdirs="$ac_ggzd_confdir"
		fi
	else
		ggzdconfdirs="$ac_ggzd_confdir $ac_ggz_stdetc"
	fi

	ggzdconfdir=NONE
	for dir in $ggzdconfdirs; do
		if test -n "`ls -d $dir/rooms 2> /dev/null`"; then
			if test -n "`ls -d $dir/rooms 2> /dev/null`"; then
				ggzdconfdir=$dir; break;
			else
				echo "tried $dir" >&AC_FD_CC;
			fi
		else
			echo "tried $dir" >&AC_FD_CC;
		fi
	done

	if test "x$ggzdconfdir" = "xNONE"; then
		have_ggzdconf="no"
	else
		have_ggzdconf="yes"
	fi
])

eval "$ac_cv_have_ggzdconf"

if test "$have_ggzdconf" != yes; then
	if test "x$2" = "xignore"; then
	  AC_MSG_RESULT([$have_ggzdconf (intentionally ignored)])
	elif test "x$2" = "xforce"; then
	  if test "x$ac_ggzd_confdir" = "x"; then
	    ggzdconfdir="\${prefix}/etc/ggzd"
	  else
	    ggzdconfdir=$ac_ggzd_confdir
	  fi
	  AC_MSG_RESULT([$have_ggzdconf (but forced to ${ggzdconfdir})])
	else
	  AC_MSG_RESULT([$have_ggzdconf])
      if test "x$2" = "x"; then
	    AC_MSG_ERROR([GGZ server configuration not found. Please check your installation! ])
      fi

	  # Perform actions given by argument 2.
	  $2
	fi
else
	prefixed=0
	if test "x${prefix}" != "xNONE" && test "x${prefix}" != "x${ac_default_prefix}"; then
		prefixed=1
	fi
	if test "x$ggzdconfdir" != "x${prefix}/etc/ggzd" && test "x$prefixed" = "x1"; then
		AC_MSG_RESULT([$have_ggzdconf ($ggzdconfdir, but using ${prefix}/etc/ggzd nevertheless)])
		ggzdconfdir="\${prefix}/etc/ggzd"
	else
		AC_MSG_RESULT([$have_ggzdconf ($ggzdconfdir)])
	fi
fi

if test "$have_ggzdconf" = yes || test "x$2" = "xforce"; then
	AC_SUBST(ggzdconfdir)

	ggzddatadir=${prefix}/share/${PACKAGE}
	AC_DEFINE_UNQUOTED(GGZDDATADIR, "${ggzddatadir}", [Game server data directory])
	AC_SUBST(ggzddatadir)

	if test "x${libdir}" = 'x${exec_prefix}/lib'; then
	  if test "x${exec_prefix}" = "xNONE"; then
	    if test "x${prefix}" = "xNONE"; then
	      ggzdexecmoddir="\${ac_default_prefix}/lib/ggzd"
		  ggzdexecmodpath="${ac_default_prefix}/lib/ggzd"
	    else
	      ggzdexecmoddir="\${prefix}/lib/ggzd"
		  ggzdexecmodpath="${prefix}/lib/ggzd"
	    fi
	  else
	    ggzdexecmoddir="\${exec_prefix}/lib/ggzd"
		ggzdexecmodpath="${exec_prefix}/lib/ggzd"
	  fi
	else
	  ggzdexecmoddir="\${libdir}/ggzd"
	  ggzdexecmodpath="${libdir}/ggzd"
	fi
	AC_SUBST(ggzdexecmoddir)
	AC_SUBST(ggzdexecmodpath)

	# Perform actions given by argument 1.
	$1
fi

])

# AC_GGZ_CHECK_SERVER
#   Check for presence of GGZ server libraries.
#
#   Simply call this function in programs that use GGZ.  GGZ_SERVER will
#   be #defined in config.h, and created as a conditional
#   in Makefile.am files, if server libraries are present.
AC_DEFUN([AC_GGZ_CHECK_SERVER],
[
  AC_GGZ_LIBGGZ([try_ggz="yes"], [try_ggz="no"])
  if test "$try_ggz" = "yes"; then
    # For now, version 0.0.14 is required.  This could be an additional
    # parameter.
    AC_GGZ_VERSION([0], [0], [14], [], [try_ggz=no])
  fi

  ggz_server="no"
  AC_ARG_WITH(ggz-server,
              AC_HELP_STRING([--with-ggz-server], [Force GGZ server support]),
              [try_ggz_server=$withval])

  if test "x$try_ggz_server" != "xno"; then
    if test "$try_ggz" = "yes"; then
      # Must pass something as the action-if-failed, or the macro will exit
      AC_GGZ_GGZDMOD([ggz_server="yes"], [ggz_server="no"])
    fi
    if test "$ggz_server" = "yes"; then
      AC_DEFINE(GGZ_SERVER, 1, [Server support for GGZ])
    else
      if test "$try_ggz_server" = "yes"; then
        AC_MSG_ERROR([Could not configure GGZ server support. See above messages.])
      fi
    fi
  fi

  AM_CONDITIONAL(GGZ_SERVER, test "$ggz_server" = "yes")
])

# AC_GGZ_CHECK
#   Check for presence of GGZ client and server libraries.
#
#   Simply call this function in programs that use GGZ.  GGZ_SERVER and
#   GGZ_CLIENT will be #defined in config.h, and created as conditionals
#   in Makefile.am files.
#
#   The only argument accepted gives the frontend for client embedding:
#      "gtk" => means the libggz-gtk library will be checked
AC_DEFUN([AC_GGZ_CHECK],
[
  AC_GGZ_INIT
  AC_GGZ_LIBGGZ([try_ggz="yes"], [try_ggz="no"])

  if test "$try_ggz" = "yes"; then
    # For now, version 0.0.14 is required.  This could be an additional
    # parameter.
    AC_GGZ_VERSION([0], [0], [14], [], [try_ggz=no])
  fi

  ggz_client="no"
  AC_ARG_WITH(ggz-client,
              AC_HELP_STRING([--with-ggz-client], [Force GGZ client support]),
              [try_ggz_client=$withval])

  if test "x$try_ggz_client" != "xno"; then
    if test "$try_ggz" = "yes"; then
      # Must pass something as the action-if-failed, or the macro will exit
      AC_GGZ_GGZMOD([AC_GGZ_CONFIG([ggz_client="yes"], [ggz_client="no"])],
                    [ggz_client="no"])
    fi
    if test "$ggz_client" = "yes"; then
      AC_DEFINE(GGZ_CLIENT, 1, [Client support for GGZ])
    else
      if test "$try_ggz_client" = "yes"; then
        AC_MSG_ERROR([Could not configure GGZ client support. See above messages.])
      fi
    fi
  fi

  ggz_gtk="no"
  if test "$ggz_client" = "yes"; then
    if test "x$1" = "xgtk"; then
      AC_GGZ_GTK([ggz_gtk="yes"])
      if test $ggz_gtk = "yes"; then
        AC_DEFINE(GGZ_GTK, 1, [Support for embedded GGZ through libggz-gtk])
      fi
    fi
  fi

  AM_CONDITIONAL(GGZ_CLIENT, test "$ggz_client" = "yes")
  AM_CONDITIONAL(GGZ_SERVER, test "$ggz_server" = "yes")
  AM_CONDITIONAL(GGZ_GTK, test "$ggz_gtk" = "yes")

  AC_GGZ_CHECK_SERVER
])
dnl
dnl GOB_HOOK(script if found, fail)
dnl if fail = "failure", abort if GOB not found
dnl


AC_DEFUN([GOB2_HOOK],[
	AC_PATH_PROG(GOB2,gob2)
	if test ! x$GOB2 = x; then	
		if test ! x$1 = x; then 
			AC_MSG_CHECKING(for gob-2 >= $1)
			g_r_ve=`echo $1|sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
			g_r_ma=`echo $1|sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
			g_r_mi=`echo $1|sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
			g_ve=`$GOB2 --version 2>&1|sed 's/Gob version \([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
			g_ma=`$GOB2 --version 2>&1|sed 's/Gob version \([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
			g_mi=`$GOB2 --version 2>&1|sed 's/Gob version \([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`

			if test $g_ve -eq $g_r_ve; then
				if test $g_ma -ge $g_r_ma; then
					if test $g_mi -ge $g_r_mi; then
						AC_MSG_RESULT(ok)
					else
						if test $g_ma -gt $g_r_ma; then
							AC_MSG_RESULT(ok)
						else
							AC_MSG_ERROR("found $g_ve.$g_ma.$g_mi requires $g_r_ve.$g_r_ma.$g_r_mi")
						fi
					fi
				else
					AC_MSG_ERROR("found $g_ve.$g_ma.$g_mi requires $g_r_ve.$g_r_ma.$g_r_mi")
				fi
			else
				if test $g_ve -gt $g_r_ve; then
					AC_MSG_RESULT(ok)
				else
					AC_MSG_ERROR(major version $g_ve found but $g_r_ve required)
				fi
			fi
	
			unset gob_version
			unset g_ve
			unset g_ma
			unset g_mi
			unset g_r_ve
			unset g_r_ma
			unset g_r_mi
		fi
		AC_SUBST(GOB2)
		$2
	else		
		$3
	fi
])

AC_DEFUN([GOB2_CHECK],[
	GOB2_HOOK($1,[],[AC_MSG_WARN([Cannot find GOB-2, check http://www.5z.com/jirka/gob.html])])
])
dnl ======================================
dnl GGZ Gaming Zone - Configuration Macros
dnl ======================================
dnl
dnl Copyright (C) 2005, 2006 Josef Spillner, josef@ggzgamingzone.org
dnl This file has been derived from libggz configuration macros.
dnl It is published under the conditions of the GNU General Public License.
dnl
dnl ======================================
dnl
dnl This file contains operating system specific checks such as for system
dnl libraries, header file flavours and non-POSIX/Unix handling.
dnl Macros defined herein are allowed to modify LDADD and LDFLAGS.
dnl Optional (advanced) checks go to system.m4 and should use LIB_* variables.
dnl
dnl ======================================
dnl
dnl History:
dnl 2005-09-14: created from libggz's configure.ac file

dnl ------------------------------------------------------------------------
dnl Content of this file:
dnl ------------------------------------------------------------------------
dnl AC_GGZ_PLATFORM_BASE - minimal required call
dnl AC_GGZ_PLATFORM_POSIX - checks for various types and functions
dnl AC_GGZ_PLATFORM_POSIX_NET - network-related checks
dnl AC_GGZ_PLATFORM_POSIX_LIBC - system libraries in addition to libc
dnl AC_GGZ_PLATFORM_WIN32 - for cross-compilation on win32 systems
dnl AC_GGZ_PLATFORM - convenience macro to call all of the above
dnl   (except for _BASE which MUST be called before AC_PROG_LIBTOOL)
dnl

dnl ------------------------------------------------------------------------
dnl AC_GGZ_PLATFORM_BASE
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_PLATFORM_BASE],
[
AC_PROG_CC
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_CPP
AC_LIBTOOL_WIN32_DLL
#AC_PROG_LIBTOOL # bug!
AC_PROG_RANLIB
AC_PROG_MAKE_SET
])

dnl ------------------------------------------------------------------------
dnl AC_GGZ_PLATFORM_WIN32
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_PLATFORM_WIN32],
[
  case $host_os in
    *mingw32* )
      MINGW32=yes
      ;;
    * )
      MINGW32=no
      ;;
  esac

  if test "$MINGW32" = "yes"; then
    LDFLAGS="$LDFLAGS -no-undefined"
    LDADD="$LDADD -lws2_32"
  fi

  AC_CHECK_HEADERS([winsock2.h])
])

dnl ------------------------------------------------------------------------
dnl AC_GGZ_PLATFORM_POSIX
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_PLATFORM_POSIX],
[
# Check for header files
# ======================
#AC_CHECK_HEADERS([fcntl.h sys/param.h],
#  [],
#  [AC_MSG_ERROR([cannot find required header file])])
#AC_CHECK_HEADERS([netdb.h sys/shm.h sys/socket.h])
#AC_CHECK_HEADERS([sys/uio.h sys/un.h sys/wait.h])
AC_CHECK_HEADERS([arpa/inet.h sys/types.h netinet/in.h])

# Check for functions
# ===================
#AC_CHECK_FUNCS([getpagesize memmove memset mkdir stat strcasecmp strerror strstr strchr],
#  [],
#  [AC_MSG_ERROR([cannot find required function])])
#AC_CHECK_FUNCS([gethostbyname socket]) # These fail; dunno why
#AC_CHECK_FUNCS([alarm getuid inet_pton])

# Check for more functions
# ========================
#AC_FUNC_FORK
##AC_FUNC_MALLOC # use ggz_malloc() always
#AC_FUNC_MEMCMP
#AC_FUNC_REALLOC
#AC_FUNC_STAT
#FUNC_MKDIR_TAKES_ONE_ARG # FIXME: external

# Check for typedefs, structures, and compiler characteristics
# ============================================================
#AC_C_CONST
#AC_TYPE_MODE_T
#AC_TYPE_PID_T
#AC_TYPE_SIGNAL
#AC_TYPE_SIZE_T
AC_C99_VARIABLE_ARRAYS # FIXME: external

# Check for debug types in syslog.h
# =================================
# If syslog is present, we take the log types from it.
# If not, define our own enumeration.
AC_CHECK_HEADER([syslog.h],
  [
    AC_DEFINE([GGZ_HAVE_SYSLOG_H], 1, [Check if syslog is present])
  ],
  [])
])

dnl ------------------------------------------------------------------------
dnl AC_GGZ_PLATFORM_POSIX_NET
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_PLATFORM_POSIX_NET],
[
# Check for sendmsg
# =================
# If sendmsg() is present, we can send file descriptors over a local socket
# via ggz_write_fd.  If not we disable this functionality.
#AC_MSG_CHECKING([for network feature: sendmsg])
AC_CHECK_FUNC([sendmsg],
  [
    AC_DEFINE([GGZ_HAVE_SENDMSG], 1, [Check if file descriptors can be sent])
  ],
  [])

# Check for PF_LOCAL/PF_UNIX
# ==========================
# There should be an autoconf macro to check this???
AC_MSG_CHECKING([for network feature: PF_LOCAL])
have_localsockets=0
AC_COMPILE_IFELSE(
  AC_LANG_PROGRAM([[#include <sys/socket.h>]], [[int a = PF_LOCAL;]]),
  [have_localsockets=1],
  [
    AC_COMPILE_IFELSE(
      AC_LANG_PROGRAM([[#include <sys/socket.h>]], [[int a = PF_UNIX;]]),
      [
        AC_DEFINE([PF_LOCAL], PF_UNIX, [PF_LOCAL is available via PF_UNIX])
        AC_DEFINE([AF_LOCAL], AF_UNIX, [AF_LOCAL is available via AF_UNIX])
        have_localsockets=1
      ],
      [])
  ])
if test "x$have_localsockets" = "x1"; then
  AC_DEFINE([GGZ_HAVE_PF_LOCAL], 1, [Check if local sockets are supported])
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi

# Check for SUN_LEN
# =================
AC_MSG_CHECKING([for network feature: SUN_LEN])
AC_LINK_IFELSE(
  AC_LANG_PROGRAM(
    [[
      #include<sys/types.h>
      #include<sys/un.h>
    ]],
    [[struct sockaddr_un su; int i = SUN_LEN(&su);]]),
  [
    AC_MSG_RESULT(yes)
    AC_DEFINE([HAVE_SUN_LEN], 1, [Define if the SUN_LEN macro exists])
  ],
  [AC_MSG_RESULT(no)])

# Check for msghdr member msg_controllen and friends
# ==================================================
AC_MSG_CHECKING([for network feature: msg_controllen])
have_controllen=0
AC_COMPILE_IFELSE(
  AC_LANG_PROGRAM([[#include <sys/socket.h>]],
    [[struct msghdr m; m.msg_controllen = 0;]]),
  [have_controllen=1],
  [])

if test "x$have_controllen" = "x1"; then
  AC_DEFINE([HAVE_MSGHDR_MSG_CONTROL], 1, [Define if msghdr has a msg_controllen member])
  AC_MSG_RESULT(yes)

  AC_MSG_CHECKING([for network feature: CMSG_ALIGN])
  AC_LINK_IFELSE(
    AC_LANG_PROGRAM(
      [[
        #include<sys/types.h>
        #include<sys/socket.h>
      ]],
      [[int i = CMSG_ALIGN(0);]]),
    [
      AC_MSG_RESULT(yes)
      AC_DEFINE([HAVE_CMSG_ALIGN], 1, [Define if CMSG_ALIGN is available])
    ],
    [AC_MSG_RESULT(no)])

  AC_MSG_CHECKING([for network feature: CMSG_LEN])
  AC_LINK_IFELSE(
    AC_LANG_PROGRAM(
      [[
        #include<sys/types.h>
        #include<sys/socket.h>
      ]],
      [[int i = CMSG_LEN(0);]]),
    [
      AC_MSG_RESULT(yes)
      AC_DEFINE([HAVE_CMSG_LEN], 1, [Define if CMSG_LEN is available])
    ],
    [AC_MSG_RESULT(no)])

  AC_MSG_CHECKING([for network feature: CMSG_SPACE])
  AC_LINK_IFELSE(
    AC_LANG_PROGRAM(
      [[
        #include<sys/types.h>
        #include<sys/socket.h>
      ]],
      [[int i = CMSG_SPACE(0);]]),
    [
      AC_MSG_RESULT(yes)
      AC_DEFINE([HAVE_CMSG_SPACE], 1, [Define if CMSG_SPACE is available])
    ],
    [AC_MSG_RESULT(no)])
else
  AC_MSG_RESULT(no)
fi
])

dnl ------------------------------------------------------------------------
dnl AC_GGZ_PLATFORM_POSIX_LIBC
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_PLATFORM_POSIX_LIBC],
[
# Check for library functions
# ===========================
# On some systems additional libraries may be needed.  Simply include them
# if they're there.
AC_CHECK_LIB(socket, socket, [LDADD="$LDADD -lsocket"])
AC_CHECK_LIB(nsl, gethostbyname, [LDADD="$LDADD -lnsl"])
])

dnl ------------------------------------------------------------------------
dnl AC_GGZ_PLATFORM
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_PLATFORM],
[
#AC_GGZ_PLATFORM_BASE
AC_GGZ_PLATFORM_POSIX
AC_GGZ_PLATFORM_POSIX_NET
AC_GGZ_PLATFORM_POSIX_LIBC
AC_GGZ_PLATFORM_WIN32
])

dnl ======================================
dnl GGZ Gaming Zone - Configuration Macros
dnl ======================================
dnl
dnl Copyright (C) 2006 Josef Spillner, josef@ggzgamingzone.org
dnl It is published under the conditions of the GNU General Public License.
dnl
dnl ======================================
dnl
dnl This file contains system library dependent macros such as asynchronous
dnl hostname lookups.
dnl As opposed to platform.m4, all optional features go here.
dnl
dnl ======================================
dnl

dnl ------------------------------------------------------------------------
dnl Content of this file:
dnl ------------------------------------------------------------------------
dnl AC_GGZ_ASYNC - find out if libanl is available which makes resolving
dnl                non-blocking
dnl                -> called automatically by AC_GGZ_PTHREADS
dnl AC_GGZ_PTHREADS - look for a pthreads implementation
dnl AC_GGZ_INTL - ensure proper i18n tools installation
dnl               -> this requires iconv.m4!
dnl

dnl ------------------------------------------------------------------------
dnl Check if asynchronous hostname lookups are possible
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_ASYNC],
[
  AC_ARG_ENABLE([anl],
    AC_HELP_STRING([--enable-anl], [Enable asynchronous hostname lookups]),
    [enable_anl=$enableval],
    [enable_anl=no])
  if test $enable_anl != "no"; then
    AC_CHECK_LIB(anl, getaddrinfo_a,
      [AC_DEFINE([GAI_A], 1, [Support for asynchronous hostname lookups])
       enable_anl=yes
       LIB_ASYNC="-lanl"],
      [enable_anl=no])
  fi

  AC_SUBST(LIB_ASYNC)
])

dnl ------------------------------------------------------------------------
dnl Check for pthreads
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_PTHREADS],
[
AC_ARG_ENABLE([threading],
  AS_HELP_STRING([--disable-threading], [Don't build in threading support]),
  [enable_threading=$enableval],
  [enable_threading=yes])

if test $enable_threading != "no"; then
  # Threading support is not available on all platforms.  It's dangerous
  # to disable it so this must be done explicitly.
  AC_CHECK_HEADER(pthread.h, [],
                  AC_MSG_ERROR([*** Cannot find pthread.h header]))
  AC_CHECK_LIB(pthread, pthread_create,
               [LDADD="$LDADD -lpthread"
                CFLAGS="$CFLAGS -D_REENTRANT"],
               [AC_CHECK_LIB(c_r, pthread_create,
                             [LDADD="$LDADD -pthread"
                              CFLAGS="$CFLAGS -D_THREAD_SAFE"],
               AC_MSG_ERROR([*** Cannot find pthread library]))])

  # If threading is available, maybe asynchronous hostname lookups
  # are as well, using libanl from glibc >= 2.3.
  AC_GGZ_ASYNC
else
  AC_DEFINE([NO_THREADING], 1, [Define if threading support is disabled])
fi

# FIXME: use 'LIBS'?
  AC_SUBST(LDADD)
])

dnl ------------------------------------------------------------------------
dnl Find internationalization tools
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_INTL],
[
AC_PATH_PROG(GETTEXT, xgettext)
AC_PATH_PROG(MSGFMT, msgfmt)
AC_PATH_PROG(MSGMERGE, msgmerge)

intl=1
if test "x$GETTEXT" = "x"; then intl=0; fi
if test "x$MSGFMT" = "x"; then intl=0; fi
if test "x$MSGMERGE" = "x"; then intl=0; fi
AM_ICONV
LIBS="$LIBICONV $LIBS"
AC_CHECK_LIB(intl, gettext, [LIBS="-lintl $LIBS"])
AC_CHECK_FUNCS([gettext ngettext], [], [intl=0])
AC_CHECK_HEADERS([libintl.h locale.h])
if test "$intl" = 0; then
  if test "x$2" = "xignore"; then
    AC_MSG_WARN([Internationalization tools missing. (ignored)])
  else
    AC_MSG_RESULT([Internationalization tools missing.])
    if test "x$2" = "x"; then
      AC_MSG_ERROR([Internationalization tools missing.])
    fi

    # Perform actions given by argument 2.
    $2
  fi
else
  AC_MSG_RESULT([Internationalization tools found.])

  XGETTEXT=$GETTEXT
  GMSGFMT=$MSGFMT

  AC_SUBST(XGETTEXT)
  AC_SUBST(GETTEXT)
  AC_SUBST(GMSGFMT)
  AC_SUBST(MSGFMT)
  AC_SUBST(MSGMERGE)

  AC_DEFINE(ENABLE_NLS, 1, [Define if NLS is enabled])

  # Perform actions given by argument 1.
  $1
fi

])

dnl ======================================
dnl GGZ Gaming Zone - Configuration Macros
dnl ======================================
dnl
dnl Copyright (C) 2001 - 2006 Josef Spillner, josef@ggzgamingzone.org
dnl This file has heavily been inspired by KDE's acinclude :)
dnl It is published under the conditions of the GNU General Public License.
dnl
dnl ======================================
dnl
dnl This file contains all autoconf macros needed for any security
dnl implementation such as TLS/SSL.
dnl
dnl ======================================
dnl
dnl History:
dnl 2002-02-10: lookup TLS libraries; taken code from acinclude.ggz
dnl 2002-02-24: default to GnuTLS; create conditional compile vars
dnl 2005-09-14: several cleanups due to newer autotools
dnl 2006-08-06: now includes gcrypt checks

dnl ------------------------------------------------------------------------
dnl Content of this file:
dnl ------------------------------------------------------------------------
dnl AC_GGZ_TLS - find a TLS implementation (support for gnutls, openssl and
dnl              none)
dnl AC_GGZ_GCRYPT - find the gcrypt encryption library
dnl
dnl Private macros:
dnl AC_PATH_SSL - OpenSSL implementation backend (code from kdelibs)
dnl AC_GGZ_GNUTLS - GNUTLS implementation backend
dnl

dnl ------------------------------------------------------------------------
dnl Find a single file
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_FIND_FILE],
[
$3=NO
for i in $2;
do
  for j in $1;
  do
    echo "configure: __oline__: $i/$j" >&AC_FD_CC
    if test -r "$i/$j"; then
      echo "taking that" >&AC_FD_CC
      $3=$i
      break 2
    fi
  done
done
])

dnl ------------------------------------------------------------------------
dnl Try to find the SSL headers and libraries.
dnl Exported are $(ssl_includes), $(ssl_libraries) and $(ssl_lib).
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_PATH_SSL],
[
LIBSSL="-lssl -lcrypto"

ac_ssl_includes=NO ac_ssl_libraries=NO
ssl_libraries=""
ssl_includes=""
AC_ARG_WITH([ssl-dir],
  AC_HELP_STRING([--with-ssl-dir=DIR], [where the root of OpenSSL is installed]),
  [
    ac_ssl_includes="$withval"/include
    ac_ssl_libraries="$withval"/lib
  ],
  [])

AC_CACHE_VAL(ac_cv_have_ssl,
[#try to guess OpenSSL locations
  
  ssl_incdirs="/usr/include /usr/local/include /usr/ssl/include /usr/local/ssl/include $prefix/include $kde_extra_includes"
  ssl_incdirs="$ac_ssl_includes $ssl_incdirs"
  AC_GGZ_FIND_FILE(openssl/ssl.h, $ssl_incdirs, ssl_incdir)
  ac_ssl_includes="$ssl_incdir"

  ssl_libdirs="/usr/lib /usr/local/lib /usr/ssl/lib /usr/local/ssl/lib $prefix/lib $exec_prefix/lib $kde_extra_libs"
  if test ! "$ac_ssl_libraries" = "NO"; then
    ssl_libdirs="$ac_ssl_libraries $ssl_libdirs"
  fi

  test=NONE
  ssl_libdir=NONE
  for dir in $ssl_libdirs; do
    try="ls -1 $dir/libssl*"
    if test=`eval $try 2> /dev/null`; then ssl_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
  done

  ac_ssl_libraries="$ssl_libdir"

  AC_LANG_SAVE
  AC_LANG_C

  ac_cflags_safe="$CFLAGS"
  ac_ldflags_safe="$LDFLAGS"
  ac_libs_safe="$LIBS"

  CFLAGS="$CFLAGS -I$ssl_incdir $all_includes"
  LDFLAGS="-L$ssl_libdir $all_libraries"
  LIBS="$LIBS $LIBSSL -lRSAglue -lrsaref"

  AC_TRY_LINK(,void RSAPrivateEncrypt(void);RSAPrivateEncrypt();,
  ac_ssl_rsaref="yes"
  ,
  ac_ssl_rsaref="no"
  )

  CFLAGS="$ac_cflags_safe"
  LDFLAGS="$ac_ldflags_safe"
  LIBS="$ac_libs_safe"

  AC_LANG_RESTORE

  if test "$ac_ssl_includes" = NO || test "$ac_ssl_libraries" = NO; then
    have_ssl=no
  else
    have_ssl=yes;
  fi

])

eval "$ac_cv_have_ssl"

if test "$have_ssl" = yes; then
  dnl Check for SSL version
  AC_CACHE_VAL(ac_cv_ssl_version,
  [
    AC_LANG_SAVE
    AC_LANG_C 

    cat >conftest.$ac_ext <<EOF
#include <openssl/opensslv.h>
#include <stdio.h>
    int main() {
 
#ifndef OPENSSL_VERSION_NUMBER
      printf("ssl_version=\\"error\\"\n");
#else
      if (OPENSSL_VERSION_NUMBER < 0x00906000)
        printf("ssl_version=\\"old\\"\n");
      else
        printf("ssl_version=\\"ok\\"\n");
#endif
     return (0);
    }
EOF

    ac_compile='${CC-gcc} $CFLAGS -I$ac_ssl_includes conftest.$ac_ext -o conftest'
    if AC_TRY_EVAL(ac_compile); then 

      if eval `./conftest 2>&5`; then
        if test $ssl_version = error; then
		  have_ssl=no
        else
          if test $ssl_version = old; then
            have_ssl=no
          fi
        fi
        ac_cv_ssl_version="ssl_version=$ssl_version"
      else
		have_ssl=no
      fi

    else
	  have_ssl=no
    fi 

    AC_LANG_RESTORE

  ])

  eval "$ac_cv_ssl_version"
fi

if test "$have_ssl" != yes; then
  LIBSSL="";
else
  AC_DEFINE(HAVE_SSL, 1, [If we are going to use OpenSSL])
  ac_cv_have_ssl="have_ssl=yes \
    ac_ssl_includes=$ac_ssl_includes ac_ssl_libraries=$ac_ssl_libraries ac_ssl_rsaref=$ac_ssl_rsaref"
  
  
  ssl_libraries="$ac_ssl_libraries"
  ssl_includes="$ac_ssl_includes"

  if test "$ac_ssl_rsaref" = yes; then
    LIBSSL="-lssl -lcrypto -lRSAglue -lrsaref" 
  fi

  if test $ssl_version = "old"; then
    AC_DEFINE(HAVE_OLD_SSL_API, 1, [Define if you have OpenSSL < 0.9.6])
  fi
fi

# got ssl_includes
# got ssl_libraries
ssl_lib=$LIBSSL

])

dnl ------------------------------------------------------------------------
dnl Try to find the GNUTLS headers and libraries.
dnl Exported are $(gnutls_includes), $(gnutls_libraries) and $(gnutls_lib).
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_GNUTLS],
[
ac_gnutls_includes=NO ac_gnutls_libraries=NO
gnutls_libraries=""
gnutls_includes=""
gnutls_lib=""

AC_ARG_WITH([gnutls-dir],
  AC_HELP_STRING([--with-gnutls-dir=DIR], [gnutls installation prefix]),
  [
    ac_gnutls_includes="$withval"/include/gnutls
    ac_gnutls_libraries="$withval"/lib
  ],
  [])
AC_ARG_WITH([gnutls-includes],
    AC_HELP_STRING([--with-gnutls-includes=DIR], [where the gnutls includes are.]),
    [
      ac_gnutls_includes="$withval"
    ],
    [])
AC_ARG_WITH([gnutls-libraries],
    AC_HELP_STRING([--with-gnutls-libraries=DIR], [where the gnutls libs are.]),
    [
      ac_gnutls_libraries="$withval"
    ],
    [])

AC_CACHE_VAL(ac_cv_have_gnutls,
[
if test "x${prefix}" = "xNONE"; then
   prefix_incdir="${ac_default_prefix}/include"
   prefix_libdir="${ac_default_prefix}/lib"
else
   prefix_incdir="${prefix}/include"
   prefix_libdir="${prefix}/lib"
fi
gnutls_incdirs="$ac_gnutls_includes $prefix_incdir/gnutls /usr/local/include/gnutls /usr/include/gnutls"
gnutls_header=gnutls.h

AC_GGZ_FIND_FILE($gnutls_header, $gnutls_incdirs, gnutls_incdir)
ac_gnutls_includes="$gnutls_incdir"

gnutls_libdirs="$ac_gnutls_libraries $prefix_libdir /usr/local/lib /usr/lib"

test=NONE
gnutls_libdir=NONE
for dir in $gnutls_libdirs; do
  try="ls -1 $dir/libgnutls.*"
  if test -n "`$try 2> /dev/null`"; then gnutls_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
done

ac_gnutls_libraries="$gnutls_libdir"

if test "$ac_gnutls_includes" = NO || test "$ac_gnutls_libraries" = NO; then
  ac_cv_have_gnutls="have_gnutls=no"
else
  have_gnutls="yes"
fi
])

eval "$ac_cv_have_gnutls"

if test "$have_gnutls" != yes; then
  have_gnutls=no
else
  ac_cv_have_gnutls="have_gnutls=yes \
    ac_gnutls_includes=$ac_gnutls_includes ac_gnutls_libraries=$ac_gnutls_libraries"

  gnutls_libraries="$ac_gnutls_libraries"
  gnutls_includes="$ac_gnutls_includes"
  gnutls_lib="-lgnutls"
fi

])

dnl ------------------------------------------------------------------------
dnl Try to find a suitable TLS implementation.
dnl Defines GGZ_TLS_NONE or GGZ_TLS_GNUTLS or GGZ_TLS_OPENSSL
dnl $(GGZTLS_LDFLAGS) will be -L ... (if needed)
dnl and $(GGZTLS_INCLUDES) will be -I ... (if needed)
dnl $(LIB_GGZTLS) will be the backend library to use against
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_TLS],
[
AC_MSG_CHECKING([for GGZ TLS implementation])

AC_ARG_WITH([tls],
  AC_HELP_STRING([--with-tls@<:@=ARG@:>@], [GnuTLS or OpenSSL - auto if no ARG]),
  [tls_type=$withval],
  [tls_type=no])

dnl None (defaults)
GGZTLS_INCLUDES=""
GGZTLS_LDFLAGS=""
LIB_GGZTLS=""
TLS_TYPE="no"

if test "$tls_type" = yes -o "$tls_type" = GnuTLS; then
  dnl GNUTLS check
  AC_GGZ_GNUTLS
  if test "$have_gnutls" = yes; then
    GGZTLS_INCLUDES="-I $gnutls_includes"
    GGZTLS_LDFLAGS="-L $gnutls_libraries"
    LIB_GGZTLS=$gnutls_lib
    AC_MSG_RESULT([using GnuTLS])
    AC_DEFINE_UNQUOTED([GGZ_TLS_GNUTLS], 1,
		       [Define if GNUTLS is to be used])
    TLS_TYPE="GnuTLS"
  fi
fi

if test \( "$tls_type" = yes -a "$have_gnutls" = no \) -o "$tls_type" = OpenSSL
then
  dnl OpenSSL check
  AC_PATH_SSL
  if test "$have_ssl" = yes; then
    GGZTLS_INCLUDES="-I $ssl_includes"
    GGZTLS_LDFLAGS="-L $ssl_libraries"
    LIB_GGZTLS=$ssl_lib
    AC_MSG_RESULT([using OpenSSL])
    AC_DEFINE_UNQUOTED([GGZ_TLS_OPENSSL], 1,
		      [Define if OpenSSL is to be used])
    TLS_TYPE="OpenSSL"
  fi
fi

if test "$TLS_TYPE" = no; then
  if test "$tls_type" = no; then
    AC_MSG_RESULT([no])
  else
    AC_MSG_WARN([No TLS implementation found - using 'no'!])
  fi
  AC_DEFINE_UNQUOTED([GGZ_TLS_NONE], 1,
		     [Define if no TLS is to be used])
fi


AC_SUBST(GGZTLS_INCLUDES)
AC_SUBST(GGZTLS_LDFLAGS)
AC_SUBST(LIB_GGZTLS)

])

dnl ------------------------------------------------------------------------
dnl Searches for the gcrypt library
dnl Defines USE_GCRYPT if found
dnl $(LIB_GCRYPT) will be set accordingly
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN([AC_GGZ_GCRYPT],
[
AC_ARG_WITH([gcrypt],
  AS_HELP_STRING([--with-gcrypt], [Add encryption support]),
  [enable_gcrypt=$enableval],
  [enable_gcrypt=auto])

if test "$enable_gcrypt" != "no"; then
  AC_CHECK_LIB(gcrypt, gcry_check_version,
    [
      AC_CHECK_HEADER(gcrypt.h,
        [
          AC_DEFINE_UNQUOTED([USE_GCRYPT], 1,
                             [Define if you have the gcrypt library])
          LIB_GCRYPT="-lgcrypt"
        ],
        [
          if test "$enable_gcrypt" = "yes"; then
            AC_MSG_ERROR([*** No gcrypt-dev found])
          else
            AC_MSG_WARN([*** No gcrypt-dev found - compiling without encryption support])
            enable_gcrypt=no
          fi
        ])
    ],
    [
      if test "$enable_gcrypt" = "yes"; then
        AC_MSG_ERROR([*** No gcrypt found])
      else
        AC_MSG_WARN([*** No gcrypt found - compiling without encryption support])
        enable_gcrypt=no
      fi
    ])
fi

AC_SUBST(LIB_GCRYPT)
])

dnl @synopsis AX_CREATE_STDINT_H [( HEADER-TO-GENERATE [, HEDERS-TO-CHECK])]
dnl
dnl the "ISO C9X: 7.18 Integer types <stdint.h>" section requires the
dnl existence of an include file <stdint.h> that defines a set of 
dnl typedefs, especially uint8_t,int32_t,uintptr_t.
dnl Many older installations will not provide this file, but some will
dnl have the very same definitions in <inttypes.h>. In other enviroments
dnl we can use the inet-types in <sys/types.h> which would define the
dnl typedefs int8_t and u_int8_t respectivly.
dnl
dnl This macros will create a local "_stdint.h" or the headerfile given as 
dnl an argument. In many cases that file will just "#include <stdint.h>" 
dnl or "#include <inttypes.h>", while in other environments it will provide 
dnl the set of basic 'stdint's definitions/typedefs: 
dnl   int8_t,uint8_t,int16_t,uint16_t,int32_t,uint32_t,intptr_t,uintptr_t
dnl   int_least32_t.. int_fast32_t.. intmax_t
dnl which may or may not rely on the definitions of other files,
dnl or using the AC_COMPILE_CHECK_SIZEOF macro to determine the actual
dnl sizeof each type.
dnl
dnl if your header files require the stdint-types you will want to create an
dnl installable file mylib-int.h that all your other installable header
dnl may include. So if you have a library package named "mylib", just use
dnl      AX_CREATE_STDINT_H(mylib-int.h) 
dnl in configure.ac and go to install that very header file in Makefile.am
dnl along with the other headers (mylib.h) - and the mylib-specific headers
dnl can simply use "#include <mylib-int.h>" to obtain the stdint-types.
dnl
dnl Remember, if the system already had a valid <stdint.h>, the generated
dnl file will include it directly. No need for fuzzy HAVE_STDINT_H things...
dnl
dnl @, (status: used on new platforms) (see http://ac-archive.sf.net/gstdint/)
dnl @version $Id$
dnl @author  Guido Draheim <guidod@gmx.de> 

AC_DEFUN([AX_CREATE_STDINT_H],
[# ------ AX CREATE STDINT H -------------------------------------
AC_MSG_CHECKING([for stdint types])
ac_stdint_h=`echo ifelse($1, , _stdint.h, $1)`
# try to shortcircuit - if the default include path of the compiler
# can find a "stdint.h" header then we assume that all compilers can.
AC_CACHE_VAL([ac_cv_header_stdint_t],[
old_CXXFLAGS="$CXXFLAGS" ; CXXFLAGS=""
old_CPPFLAGS="$CPPFLAGS" ; CPPFLAGS=""
old_CFLAGS="$CFLAGS"     ; CFLAGS=""
AC_TRY_COMPILE([#include <stdint.h>],[int_least32_t v = 0;],
[ac_cv_stdint_result="(assuming C99 compatible system)"
 ac_cv_header_stdint_t="stdint.h"; ],
[ac_cv_header_stdint_t=""])
CXXFLAGS="$old_CXXFLAGS"
CPPFLAGS="$old_CPPFLAGS"
CFLAGS="$old_CFLAGS" ])

v="... $ac_cv_header_stdint_h"
if test "$ac_stdint_h" = "stdint.h" ; then
 AC_MSG_RESULT([(are you sure you want them in ./stdint.h?)])
elif test "$ac_stdint_h" = "inttypes.h" ; then
 AC_MSG_RESULT([(are you sure you want them in ./inttypes.h?)])
elif test "_$ac_cv_header_stdint_t" = "_" ; then
 AC_MSG_RESULT([(putting them into $ac_stdint_h)$v])
else
 ac_cv_header_stdint="$ac_cv_header_stdint_t"
 AC_MSG_RESULT([$ac_cv_header_stdint (shortcircuit)])
fi

if test "_$ac_cv_header_stdint_t" = "_" ; then # can not shortcircuit..

dnl .....intro message done, now do a few system checks.....
dnl btw, all CHECK_TYPE macros do automatically "DEFINE" a type, therefore
dnl we use the autoconf implementation detail _AC CHECK_TYPE_NEW instead

inttype_headers=`echo $2 | sed -e 's/,/ /g'`

ac_cv_stdint_result="(no helpful system typedefs seen)"
AC_CACHE_CHECK([for stdint uintptr_t], [ac_cv_header_stdint_x],[
 ac_cv_header_stdint_x="" # the 1997 typedefs (inttypes.h)
  AC_MSG_RESULT([(..)])
  for i in stdint.h inttypes.h sys/inttypes.h $inttype_headers ; do
   unset ac_cv_type_uintptr_t 
   unset ac_cv_type_uint64_t
   _AC_CHECK_TYPE_NEW(uintptr_t,[ac_cv_header_stdint_x=$i],dnl
     continue,[#include <$i>])
   AC_CHECK_TYPE(uint64_t,[and64="/uint64_t"],[and64=""],[#include<$i>])
   ac_cv_stdint_result="(seen uintptr_t$and64 in $i)"
   break;
  done
  AC_MSG_CHECKING([for stdint uintptr_t])
 ])

if test "_$ac_cv_header_stdint_x" = "_" ; then
AC_CACHE_CHECK([for stdint uint32_t], [ac_cv_header_stdint_o],[
 ac_cv_header_stdint_o="" # the 1995 typedefs (sys/inttypes.h)
  AC_MSG_RESULT([(..)])
  for i in inttypes.h sys/inttypes.h stdint.h $inttype_headers ; do
   unset ac_cv_type_uint32_t
   unset ac_cv_type_uint64_t
   AC_CHECK_TYPE(uint32_t,[ac_cv_header_stdint_o=$i],dnl
     continue,[#include <$i>])
   AC_CHECK_TYPE(uint64_t,[and64="/uint64_t"],[and64=""],[#include<$i>])
   ac_cv_stdint_result="(seen uint32_t$and64 in $i)"
   break;
  done
  AC_MSG_CHECKING([for stdint uint32_t])
 ])
fi

if test "_$ac_cv_header_stdint_x" = "_" ; then
if test "_$ac_cv_header_stdint_o" = "_" ; then
AC_CACHE_CHECK([for stdint u_int32_t], [ac_cv_header_stdint_u],[
 ac_cv_header_stdint_u="" # the BSD typedefs (sys/types.h)
  AC_MSG_RESULT([(..)])
  for i in sys/types.h inttypes.h sys/inttypes.h $inttype_headers ; do
   unset ac_cv_type_u_int32_t
   unset ac_cv_type_u_int64_t
   AC_CHECK_TYPE(u_int32_t,[ac_cv_header_stdint_u=$i],dnl
     continue,[#include <$i>])
   AC_CHECK_TYPE(u_int64_t,[and64="/u_int64_t"],[and64=""],[#include<$i>])
   ac_cv_stdint_result="(seen u_int32_t$and64 in $i)"
   break;
  done
  AC_MSG_CHECKING([for stdint u_int32_t])
 ])
fi fi

dnl if there was no good C99 header file, do some typedef checks...
if test "_$ac_cv_header_stdint_x" = "_" ; then
   AC_MSG_CHECKING([for stdint datatype model])
   AC_MSG_RESULT([(..)])
   AC_COMPILE_CHECK_SIZEOF(char)
   AC_COMPILE_CHECK_SIZEOF(short)
   AC_COMPILE_CHECK_SIZEOF(int)
   AC_COMPILE_CHECK_SIZEOF(long)
   AC_COMPILE_CHECK_SIZEOF(void*)
   ac_cv_stdint_char_model=""
   ac_cv_stdint_char_model="$ac_cv_stdint_char_model$ac_cv_sizeof_char"
   ac_cv_stdint_char_model="$ac_cv_stdint_char_model$ac_cv_sizeof_short"
   ac_cv_stdint_char_model="$ac_cv_stdint_char_model$ac_cv_sizeof_int"
   ac_cv_stdint_long_model=""
   ac_cv_stdint_long_model="$ac_cv_stdint_long_model$ac_cv_sizeof_int"
   ac_cv_stdint_long_model="$ac_cv_stdint_long_model$ac_cv_sizeof_long"
   ac_cv_stdint_long_model="$ac_cv_stdint_long_model$ac_cv_sizeof_voidp"
   name="$ac_cv_stdint_long_model"
   case "$ac_cv_stdint_char_model/$ac_cv_stdint_long_model" in
    122/242)     name="$name,  IP16 (standard 16bit machine)" ;;
    122/244)     name="$name,  LP32 (standard 32bit mac/win)" ;;
    122/*)       name="$name        (unusual int16 model)" ;; 
    124/444)     name="$name, ILP32 (standard 32bit unixish)" ;;
    124/488)     name="$name,  LP64 (standard 64bit unixish)" ;;
    124/448)     name="$name, LLP64 (unusual  64bit unixish)" ;;
    124/*)       name="$name        (unusual int32 model)" ;; 
    128/888)     name="$name, ILP64 (unusual  64bit numeric)" ;;
    128/*)       name="$name        (unusual int64 model)" ;; 
    222/*|444/*) name="$name        (unusual dsptype)" ;;
     *)          name="$name        (very unusal model)" ;;
   esac
   AC_MSG_RESULT([combined for stdint datatype model...  $name])
fi

if test "_$ac_cv_header_stdint_x" != "_" ; then
   ac_cv_header_stdint="$ac_cv_header_stdint_x"
elif  test "_$ac_cv_header_stdint_o" != "_" ; then
   ac_cv_header_stdint="$ac_cv_header_stdint_o"
elif  test "_$ac_cv_header_stdint_u" != "_" ; then
   ac_cv_header_stdint="$ac_cv_header_stdint_u"
else
   ac_cv_header_stdint="stddef.h"
fi

AC_MSG_CHECKING([for extra inttypes in chosen header])
AC_MSG_RESULT([($ac_cv_header_stdint)])
dnl see if int_least and int_fast types are present in _this_ header.
unset ac_cv_type_int_least32_t
unset ac_cv_type_int_fast32_t
AC_CHECK_TYPE(int_least32_t,,,[#include <$ac_cv_header_stdint>])
AC_CHECK_TYPE(int_fast32_t,,,[#include<$ac_cv_header_stdint>])
AC_CHECK_TYPE(intmax_t,,,[#include <$ac_cv_header_stdint>])

fi # shortcircut to system "stdint.h"
# ------------------ PREPARE VARIABLES ------------------------------
if test "$GCC" = "yes" ; then
ac_cv_stdint_message="using gnu compiler "`$CC --version | head -1` 
else
ac_cv_stdint_message="using $CC"
fi

AC_MSG_RESULT([make use of $ac_cv_header_stdint in $ac_stdint_h dnl
$ac_cv_stdint_result])

# ----------------- DONE inttypes.h checks START header -------------
AC_CONFIG_COMMANDS([$ac_stdint_h],[
AC_MSG_NOTICE(creating $ac_stdint_h : $_ac_stdint_h)
ac_stdint=$tmp/_stdint.h

echo "#ifndef" $_ac_stdint_h >$ac_stdint
echo "#define" $_ac_stdint_h "1" >>$ac_stdint
echo "#ifndef" _GENERATED_STDINT_H >>$ac_stdint
echo "#define" _GENERATED_STDINT_H '"'$PACKAGE $VERSION'"' >>$ac_stdint
echo "/* generated $ac_cv_stdint_message */" >>$ac_stdint
if test "_$ac_cv_header_stdint_t" != "_" ; then 
echo "#define _STDINT_HAVE_STDINT_H" "1" >>$ac_stdint
fi

cat >>$ac_stdint <<STDINT_EOF

/* ................... shortcircuit part ........................... */

#if defined HAVE_STDINT_H || defined _STDINT_HAVE_STDINT_H
#include <stdint.h>
#else
#include <stddef.h>

/* .................... configured part ............................ */

STDINT_EOF

echo "/* whether we have a C99 compatible stdint header file */" >>$ac_stdint
if test "_$ac_cv_header_stdint_x" != "_" ; then
  ac_header="$ac_cv_header_stdint_x"
  echo "#define _STDINT_HEADER_INTPTR" '"'"$ac_header"'"' >>$ac_stdint
else
  echo "/* #undef _STDINT_HEADER_INTPTR */" >>$ac_stdint
fi

echo "/* whether we have a C96 compatible inttypes header file */" >>$ac_stdint
if  test "_$ac_cv_header_stdint_o" != "_" ; then
  ac_header="$ac_cv_header_stdint_o"
  echo "#define _STDINT_HEADER_UINT32" '"'"$ac_header"'"' >>$ac_stdint
else
  echo "/* #undef _STDINT_HEADER_UINT32 */" >>$ac_stdint
fi

echo "/* whether we have a BSD compatible inet types header */" >>$ac_stdint
if  test "_$ac_cv_header_stdint_u" != "_" ; then
  ac_header="$ac_cv_header_stdint_u"
  echo "#define _STDINT_HEADER_U_INT32" '"'"$ac_header"'"' >>$ac_stdint
else
  echo "/* #undef _STDINT_HEADER_U_INT32 */" >>$ac_stdint
fi

echo "" >>$ac_stdint

if test "_$ac_header" != "_" ; then if test "$ac_header" != "stddef.h" ; then
  echo "#include <$ac_header>" >>$ac_stdint
  echo "" >>$ac_stdint
fi fi

echo "/* which 64bit typedef has been found */" >>$ac_stdint
if test "$ac_cv_type_uint64_t" = "yes" ; then
echo "#define   _STDINT_HAVE_UINT64_T" "1"  >>$ac_stdint
else
echo "/* #undef _STDINT_HAVE_UINT64_T */" >>$ac_stdint
fi
if test "$ac_cv_type_u_int64_t" = "yes" ; then
echo "#define   _STDINT_HAVE_U_INT64_T" "1"  >>$ac_stdint
else
echo "/* #undef _STDINT_HAVE_U_INT64_T */" >>$ac_stdint
fi
echo "" >>$ac_stdint

echo "/* which type model has been detected */" >>$ac_stdint
if test "_$ac_cv_stdint_char_model" != "_" ; then
echo "#define   _STDINT_CHAR_MODEL" "$ac_cv_stdint_char_model" >>$ac_stdint
echo "#define   _STDINT_LONG_MODEL" "$ac_cv_stdint_long_model" >>$ac_stdint
else
echo "/* #undef _STDINT_CHAR_MODEL // skipped */" >>$ac_stdint
echo "/* #undef _STDINT_LONG_MODEL // skipped */" >>$ac_stdint
fi
echo "" >>$ac_stdint

echo "/* whether int_least types were detected */" >>$ac_stdint
if test "$ac_cv_type_int_least32_t" = "yes"; then
echo "#define   _STDINT_HAVE_INT_LEAST32_T" "1"  >>$ac_stdint
else
echo "/* #undef _STDINT_HAVE_INT_LEAST32_T */" >>$ac_stdint
fi
echo "/* whether int_fast types were detected */" >>$ac_stdint
if test "$ac_cv_type_int_fast32_t" = "yes"; then
echo "#define   _STDINT_HAVE_INT_FAST32_T" "1" >>$ac_stdint
else
echo "/* #undef _STDINT_HAVE_INT_FAST32_T */" >>$ac_stdint
fi
echo "/* whether intmax_t type was detected */" >>$ac_stdint
if test "$ac_cv_type_intmax_t" = "yes"; then
echo "#define   _STDINT_HAVE_INTMAX_T" "1" >>$ac_stdint
else
echo "/* #undef _STDINT_HAVE_INTMAX_T */" >>$ac_stdint
fi
echo "" >>$ac_stdint

  cat >>$ac_stdint <<STDINT_EOF
/* .................... detections part ............................ */

/* whether we need to define bitspecific types from compiler base types */
#ifndef _STDINT_HEADER_INTPTR
#ifndef _STDINT_HEADER_UINT32
#ifndef _STDINT_HEADER_U_INT32
#define _STDINT_NEED_INT_MODEL_T
#else
#define _STDINT_HAVE_U_INT_TYPES
#endif
#endif
#endif

#ifdef _STDINT_HAVE_U_INT_TYPES
#undef _STDINT_NEED_INT_MODEL_T
#endif

#ifdef  _STDINT_CHAR_MODEL
#if     _STDINT_CHAR_MODEL+0 == 122 || _STDINT_CHAR_MODEL+0 == 124
#ifndef _STDINT_BYTE_MODEL
#define _STDINT_BYTE_MODEL 12
#endif
#endif
#endif

#ifndef _STDINT_HAVE_INT_LEAST32_T
#define _STDINT_NEED_INT_LEAST_T
#endif

#ifndef _STDINT_HAVE_INT_FAST32_T
#define _STDINT_NEED_INT_FAST_T
#endif

#ifndef _STDINT_HEADER_INTPTR
#define _STDINT_NEED_INTPTR_T
#ifndef _STDINT_HAVE_INTMAX_T
#define _STDINT_NEED_INTMAX_T
#endif
#endif


/* .................... definition part ............................ */

/* some system headers have good uint64_t */
#ifndef _HAVE_UINT64_T
#if     defined _STDINT_HAVE_UINT64_T  || defined HAVE_UINT64_T
#define _HAVE_UINT64_T
#elif   defined _STDINT_HAVE_U_INT64_T || defined HAVE_U_INT64_T
#define _HAVE_UINT64_T
typedef u_int64_t uint64_t;
#endif
#endif

#ifndef _HAVE_UINT64_T
/* .. here are some common heuristics using compiler runtime specifics */
#if defined __STDC_VERSION__ && defined __STDC_VERSION__ >= 199901L
#define _HAVE_UINT64_T
typedef long long int64_t;
typedef unsigned long long uint64_t;

#elif !defined __STRICT_ANSI__
#if defined _MSC_VER || defined __WATCOMC__ || defined __BORLANDC__
#define _HAVE_UINT64_T
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;

#elif defined __GNUC__ || defined __MWERKS__ || defined __ELF__
/* note: all ELF-systems seem to have loff-support which needs 64-bit */
#if !defined _NO_LONGLONG
#define _HAVE_UINT64_T
typedef long long int64_t;
typedef unsigned long long uint64_t;
#endif

#elif defined __alpha || (defined __mips && defined _ABIN32)
#if !defined _NO_LONGLONG
typedef long int64_t;
typedef unsigned long uint64_t;
#endif
  /* compiler/cpu type to define int64_t */
#endif
#endif
#endif

#if defined _STDINT_HAVE_U_INT_TYPES
/* int8_t int16_t int32_t defined by inet code, redeclare the u_intXX types */
typedef u_int8_t uint8_t;
typedef u_int16_t uint16_t;
typedef u_int32_t uint32_t;

/* glibc compatibility */
#ifndef __int8_t_defined
#define __int8_t_defined
#endif
#endif

#ifdef _STDINT_NEED_INT_MODEL_T
/* we must guess all the basic types. Apart from byte-adressable system, */
/* there a few 32-bit-only dsp-systems that we guard with BYTE_MODEL 8-} */
/* (btw, those nibble-addressable systems are way off, or so we assume) */

dnl   /* have a look at "64bit and data size neutrality" at */
dnl   /* http://unix.org/version2/whatsnew/login_64bit.html */
dnl   /* (the shorthand "ILP" types always have a "P" part) */

#if defined _STDINT_BYTE_MODEL
#if _STDINT_LONG_MODEL+0 == 242
/* 2:4:2 =  IP16 = a normal 16-bit system                */
typedef unsigned char   uint8_t;
typedef unsigned short  uint16_t;
typedef unsigned long   uint32_t;
#ifndef __int8_t_defined
#define __int8_t_defined
typedef          char    int8_t;
typedef          short   int16_t;
typedef          long    int32_t;
#endif
#elif _STDINT_LONG_MODEL+0 == 244 || _STDINT_LONG_MODEL == 444
/* 2:4:4 =  LP32 = a 32-bit system derived from a 16-bit */
/* 4:4:4 = ILP32 = a normal 32-bit system                */
typedef unsigned char   uint8_t;
typedef unsigned short  uint16_t;
typedef unsigned int    uint32_t;
#ifndef __int8_t_defined
#define __int8_t_defined
typedef          char    int8_t;
typedef          short   int16_t;
typedef          int     int32_t;
#endif
#elif _STDINT_LONG_MODEL+0 == 484 || _STDINT_LONG_MODEL+0 == 488
/* 4:8:4 =  IP32 = a 32-bit system prepared for 64-bit    */
/* 4:8:8 =  LP64 = a normal 64-bit system                 */
typedef unsigned char   uint8_t;
typedef unsigned short  uint16_t;
typedef unsigned int    uint32_t;
#ifndef __int8_t_defined
#define __int8_t_defined
typedef          char    int8_t;
typedef          short   int16_t;
typedef          int     int32_t;
#endif
/* this system has a "long" of 64bit */
#ifndef _HAVE_UINT64_T
#define _HAVE_UINT64_T
typedef unsigned long   uint64_t;
typedef          long    int64_t;
#endif
#elif _STDINT_LONG_MODEL+0 == 448
/*      LLP64   a 64-bit system derived from a 32-bit system */
typedef unsigned char   uint8_t;
typedef unsigned short  uint16_t;
typedef unsigned int    uint32_t;
#ifndef __int8_t_defined
#define __int8_t_defined
typedef          char    int8_t;
typedef          short   int16_t;
typedef          int     int32_t;
#endif
/* assuming the system has a "long long" */
#ifndef _HAVE_UINT64_T
#define _HAVE_UINT64_T
typedef unsigned long long uint64_t;
typedef          long long  int64_t;
#endif
#else
#define _STDINT_NO_INT32_T
#endif
#else
#define _STDINT_NO_INT8_T
#define _STDINT_NO_INT32_T
#endif
#endif

/*
 * quote from SunOS-5.8 sys/inttypes.h:
 * Use at your own risk.  As of February 1996, the committee is squarely
 * behind the fixed sized types; the "least" and "fast" types are still being
 * discussed.  The probability that the "fast" types may be removed before
 * the standard is finalized is high enough that they are not currently
 * implemented.
 */

#if defined _STDINT_NEED_INT_LEAST_T
typedef  int8_t    int_least8_t;
typedef  int16_t   int_least16_t;
typedef  int32_t   int_least32_t;
#ifdef _HAVE_UINT64_T
typedef  int64_t   int_least64_t;
#endif

typedef uint8_t   uint_least8_t;
typedef uint16_t  uint_least16_t;
typedef uint32_t  uint_least32_t;
#ifdef _HAVE_UINT64_T
typedef uint64_t  uint_least64_t;
#endif
  /* least types */
#endif

#if defined _STDINT_NEED_INT_FAST_T
typedef  int8_t    int_fast8_t; 
typedef  int       int_fast16_t;
typedef  int32_t   int_fast32_t;
#ifdef _HAVE_UINT64_T
typedef  int64_t   int_fast64_t;
#endif

typedef uint8_t   uint_fast8_t; 
typedef unsigned  uint_fast16_t;
typedef uint32_t  uint_fast32_t;
#ifdef _HAVE_UINT64_T
typedef uint64_t  uint_fast64_t;
#endif
  /* fast types */
#endif

#ifdef _STDINT_NEED_INTMAX_T
#ifdef _HAVE_UINT64_T
typedef  int64_t       intmax_t;
typedef uint64_t      uintmax_t;
#else
typedef          long  intmax_t;
typedef unsigned long uintmax_t;
#endif
#endif

#ifdef _STDINT_NEED_INTPTR_T
#ifndef __intptr_t_defined
#define __intptr_t_defined
/* we encourage using "long" to store pointer values, never use "int" ! */
#if   _STDINT_LONG_MODEL+0 == 242 || _STDINT_LONG_MODEL+0 == 484
typedef  unsinged int   uintptr_t;
typedef           int    intptr_t;
#elif _STDINT_LONG_MODEL+0 == 244 || _STDINT_LONG_MODEL+0 == 444
typedef  unsigned long  uintptr_t;
typedef           long   intptr_t;
#elif _STDINT_LONG_MODEL+0 == 448 && defined _HAVE_UINT64_T
typedef        uint64_t uintptr_t;
typedef         int64_t  intptr_t;
#else /* matches typical system types ILP32 and LP64 - but not IP16 or LLP64 */
typedef  unsigned long  uintptr_t;
typedef           long   intptr_t;
#endif
#endif
#endif

  /* shortcircuit*/
#endif
  /* once */
#endif
#endif
STDINT_EOF
    if cmp -s $ac_stdint_h $ac_stdint 2>/dev/null; then
      AC_MSG_NOTICE([$ac_stdint_h is unchanged])
    else
      ac_dir=`AS_DIRNAME(["$ac_stdint_h"])`
      AS_MKDIR_P(["$ac_dir"])
      rm -f $ac_stdint_h
      mv $ac_stdint $ac_stdint_h
    fi
],[# variables for create stdint.h replacement
PACKAGE="$PACKAGE"
VERSION="$VERSION"
ac_stdint_h="$ac_stdint_h"
_ac_stdint_h=AS_TR_CPP(_$PACKAGE-$ac_stdint_h)
ac_cv_stdint_message="$ac_cv_stdint_message"
ac_cv_header_stdint_t="$ac_cv_header_stdint_t"
ac_cv_header_stdint_x="$ac_cv_header_stdint_x"
ac_cv_header_stdint_o="$ac_cv_header_stdint_o"
ac_cv_header_stdint_u="$ac_cv_header_stdint_u"
ac_cv_type_uint64_t="$ac_cv_type_uint64_t"
ac_cv_type_u_int64_t="$ac_cv_type_u_int64_t"
ac_cv_stdint_char_model="$ac_cv_stdint_char_model"
ac_cv_stdint_long_model="$ac_cv_stdint_long_model"
ac_cv_type_int_least32_t="$ac_cv_type_int_least32_t"
ac_cv_type_int_fast32_t="$ac_cv_type_int_fast32_t"
ac_cv_type_intmax_t="$ac_cv_type_intmax_t"
])
])
dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/ac_compile_check_sizeof.html
dnl
AC_DEFUN([AC_COMPILE_CHECK_SIZEOF],
[changequote(<<, >>)dnl
dnl The name to #define.
define(<<AC_TYPE_NAME>>, translit(sizeof_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name.
define(<<AC_CV_NAME>>, translit(ac_cv_sizeof_$1, [ *], [_p]))dnl
changequote([, ])dnl
AC_MSG_CHECKING(size of $1)
AC_CACHE_VAL(AC_CV_NAME,
[for ac_size in 4 8 1 2 16 $2 ; do # List sizes in rough order of prevalence.
  AC_TRY_COMPILE([#include "confdefs.h"
#include <sys/types.h>
$2
], [switch (0) case 0: case (sizeof ($1) == $ac_size):;], AC_CV_NAME=$ac_size)
  if test x$AC_CV_NAME != x ; then break; fi
done
])
if test x$AC_CV_NAME = x ; then
  AC_MSG_ERROR([cannot determine a size for $1])
fi
AC_MSG_RESULT($AC_CV_NAME)
AC_DEFINE_UNQUOTED(AC_TYPE_NAME, $AC_CV_NAME, [The number of bytes in type $1])
undefine([AC_TYPE_NAME])dnl
undefine([AC_CV_NAME])dnl
])
dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/acx_pthread.html
dnl
AC_DEFUN([ACX_PTHREAD], [
AC_REQUIRE([AC_CANONICAL_HOST])
AC_LANG_SAVE
AC_LANG_C
acx_pthread_ok=no

# We used to check for pthread.h first, but this fails if pthread.h
# requires special compiler flags (e.g. on True64 or Sequent).
# It gets checked for in the link test anyway.

# First of all, check if the user has set any of the PTHREAD_LIBS,
# etcetera environment variables, and if threads linking works using
# them:
if test x"$PTHREAD_LIBS$PTHREAD_CFLAGS" != x; then
        save_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
        save_LIBS="$LIBS"
        LIBS="$PTHREAD_LIBS $LIBS"
        AC_MSG_CHECKING([for pthread_join in LIBS=$PTHREAD_LIBS with CFLAGS=$PTHREAD_CFLAGS])
        AC_TRY_LINK_FUNC(pthread_join, acx_pthread_ok=yes)
        AC_MSG_RESULT($acx_pthread_ok)
        if test x"$acx_pthread_ok" = xno; then
                PTHREAD_LIBS=""
                PTHREAD_CFLAGS=""
        fi
        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"
fi

# We must check for the threads library under a number of different
# names; the ordering is very important because some systems
# (e.g. DEC) have both -lpthread and -lpthreads, where one of the
# libraries is broken (non-POSIX).

# Create a list of thread flags to try.  Items starting with a "-" are
# C compiler flags, and other items are library names, except for "none"
# which indicates that we try without any flags at all.

acx_pthread_flags="pthreads none -Kthread -kthread lthread -pthread -pthreads -mthreads pthread --thread-safe -mt"

# The ordering *is* (sometimes) important.  Some notes on the
# individual items follow:

# pthreads: AIX (must check this before -lpthread)
# none: in case threads are in libc; should be tried before -Kthread and
#       other compiler flags to prevent continual compiler warnings
# -Kthread: Sequent (threads in libc, but -Kthread needed for pthread.h)
# -kthread: FreeBSD kernel threads (preferred to -pthread since SMP-able)
# lthread: LinuxThreads port on FreeBSD (also preferred to -pthread)
# -pthread: Linux/gcc (kernel threads), BSD/gcc (userland threads)
# -pthreads: Solaris/gcc
# -mthreads: Mingw32/gcc, Lynx/gcc
# -mt: Sun Workshop C (may only link SunOS threads [-lthread], but it
#      doesn't hurt to check since this sometimes defines pthreads too;
#      also defines -D_REENTRANT)
# pthread: Linux, etcetera
# --thread-safe: KAI C++

case "${host_cpu}-${host_os}" in
        *solaris*)

        # On Solaris (at least, for some versions), libc contains stubbed
        # (non-functional) versions of the pthreads routines, so link-based
        # tests will erroneously succeed.  (We need to link with -pthread or
        # -lpthread.)  (The stubs are missing pthread_cleanup_push, or rather
        # a function called by this macro, so we could check for that, but
        # who knows whether they'll stub that too in a future libc.)  So,
        # we'll just look for -pthreads and -lpthread first:

        acx_pthread_flags="-pthread -pthreads pthread -mt $acx_pthread_flags"
        ;;
esac

if test x"$acx_pthread_ok" = xno; then
for flag in $acx_pthread_flags; do

        case $flag in
                none)
                AC_MSG_CHECKING([whether pthreads work without any flags])
                ;;

                -*)
                AC_MSG_CHECKING([whether pthreads work with $flag])
                PTHREAD_CFLAGS="$flag"
                ;;

                *)
                AC_MSG_CHECKING([for the pthreads library -l$flag])
                PTHREAD_LIBS="-l$flag"
                ;;
        esac

        save_LIBS="$LIBS"
        save_CFLAGS="$CFLAGS"
        LIBS="$PTHREAD_LIBS $LIBS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"

        # Check for various functions.  We must include pthread.h,
        # since some functions may be macros.  (On the Sequent, we
        # need a special flag -Kthread to make this header compile.)
        # We check for pthread_join because it is in -lpthread on IRIX
        # while pthread_create is in libc.  We check for pthread_attr_init
        # due to DEC craziness with -lpthreads.  We check for
        # pthread_cleanup_push because it is one of the few pthread
        # functions on Solaris that doesn't have a non-functional libc stub.
        # We try pthread_create on general principles.
        AC_TRY_LINK([#include <pthread.h>],
                    [pthread_t th; pthread_join(th, 0);
                     pthread_attr_init(0); pthread_cleanup_push(0, 0);
                     pthread_create(0,0,0,0); pthread_cleanup_pop(0); ],
                    [acx_pthread_ok=yes])

        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"

        AC_MSG_RESULT($acx_pthread_ok)
        if test "x$acx_pthread_ok" = xyes; then
                break;
        fi

        PTHREAD_LIBS=""
        PTHREAD_CFLAGS=""
done
fi

# Various other checks:
if test "x$acx_pthread_ok" = xyes; then
        save_LIBS="$LIBS"
        LIBS="$PTHREAD_LIBS $LIBS"
        save_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"

        # Detect AIX lossage: threads are created detached by default
        # and the JOINABLE attribute has a nonstandard name (UNDETACHED).
        AC_MSG_CHECKING([for joinable pthread attribute])
        AC_TRY_LINK([#include <pthread.h>],
                    [int attr=PTHREAD_CREATE_JOINABLE;],
                    ok=PTHREAD_CREATE_JOINABLE, ok=unknown)
        if test x"$ok" = xunknown; then
                AC_TRY_LINK([#include <pthread.h>],
                            [int attr=PTHREAD_CREATE_UNDETACHED;],
                            ok=PTHREAD_CREATE_UNDETACHED, ok=unknown)
        fi
        if test x"$ok" != xPTHREAD_CREATE_JOINABLE; then
                AC_DEFINE(PTHREAD_CREATE_JOINABLE, $ok,
                          [Define to the necessary symbol if this constant
                           uses a non-standard name on your system.])
        fi
        AC_MSG_RESULT(${ok})
        if test x"$ok" = xunknown; then
                AC_MSG_WARN([we do not know how to create joinable pthreads])
        fi

        AC_MSG_CHECKING([if more special flags are required for pthreads])
        flag=no
        case "${host_cpu}-${host_os}" in
                *-aix* | *-freebsd*)     flag="-D_THREAD_SAFE";;
                *solaris* | *-osf* | *-hpux*) flag="-D_REENTRANT";;
        esac
        AC_MSG_RESULT(${flag})
        if test "x$flag" != xno; then
                PTHREAD_CFLAGS="$flag $PTHREAD_CFLAGS"
        fi

        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"

        # More AIX lossage: must compile with cc_r
        AC_CHECK_PROG(PTHREAD_CC, cc_r, cc_r, ${CC})
else
        PTHREAD_CC="$CC"
fi

AC_SUBST(PTHREAD_LIBS)
AC_SUBST(PTHREAD_CFLAGS)
AC_SUBST(PTHREAD_CC)

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_pthread_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_PTHREAD,1,[Define if you have POSIX threads libraries and header files.]),[$1])
        :
else
        acx_pthread_ok=no
        $2
fi
AC_LANG_RESTORE
])dnl ACX_PTHREAD
