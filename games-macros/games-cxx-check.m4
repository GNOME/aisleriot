dnl GAMES_CHECK_CXX(not_found_string)
AC_DEFUN(GAMES_CHECK_CXX,
[
  # see if a C++ compiler exists and works
  AC_REQUIRE([AC_PROG_CXX])dnl

  AC_CHECK_PROG(ac_cv_prog_cxx_works, $CXX, yes, no)

  AM_CONDITIONAL(CXX_PRESENT, test "x$ac_cv_prog_cxx_works" != xno)
])

