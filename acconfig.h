#undef HAVE_LC_MESSAGES
#undef HAVE_STPCPY
#undef HAVE_LIBSM
#undef HAVE_GUILE

/*=== Curses version detection defines ===*/
/* Found some version of curses that we're going to use */
#undef HAS_CURSES
   
/* Use SunOS SysV curses? */
#undef USE_SUNOS_CURSES

/* Use old BSD curses - not used right now */
#undef USE_BSD_CURSES

/* Use SystemV curses? */
#undef USE_SYSV_CURSES

/* Use Ncurses? */
#undef USE_NCURSES

/* If you Curses does not have color define this one */
#undef NO_COLOR_CURSES

/* Set to reflect version of ncurses *
 *   0 = version 1.*
 *   1 = version 1.9.9g
 *   2 = version 4.0/4.1 */
#undef NCURSES_970530

/* Define if scandir() works with struct direct rather than struct dirent */
#undef HAVE_STRUCT_DIRECT

#undef SCO_FLAVOR
