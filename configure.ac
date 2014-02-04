#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([guile-wm], [1.0], [mark@markwitmer.com])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options])

AC_CONFIG_FILES([
        Makefile
        module/Makefile
])

GUILE_PKG([2.0 2.2])
GUILE_SITE_DIR
GUILE_PROGS

GUILE_MODULE_REQUIRED([xcb xml])
GUILE_MODULE_REQUIRED([xcb xml xproto])
GUILE_MODULE_REQUIRED([xcb xml struct])
GUILE_MODULE_REQUIRED([xcb xml ext randr])
GUILE_MODULE_REQUIRED([xcb event-loop])

AC_ARG_WITH([guile-site-dir],
        [AS_HELP_STRING([--with-guile-site-dir],
            [use the specified installation path for Guile modules]
        )],
        [guilesitedir="$withval"],
        [guilesitedir="$GUILE_SITE"]
)

AC_SUBST([guilesitedir])
AC_MSG_RESULT($guilesitedir)

AC_OUTPUT
