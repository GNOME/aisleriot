dnl GAMES_CHECK_CXX(not_found_string)
AC_DEFUN(GAMES_CHECK_CXX,
[
  # see if a C++ compiler exists and works
  AC_REQUIRE([AC_PROG_CXX])dnl

  # If this is not GNU C++ but CXX = g++ then no compiles was found.


  if test "x$GXX" = xyes && test "x$CXX" = xg++; then
    ac_cv_prog_cxx_works=yes 
  else
    AC_MSG_WARN(ifelse([$1], , "No C++ compiler", [$1]))
    ac_cv_prog_cxx_works=no  
  fi

  AM_CONDITIONAL(CXX_PRESENT, test "x$ac_cv_prog_cxx_works" != xno)
])

