/* freecell.c
   Copyright (C) 1997 Ryu Changwoo

   This program is free software; you can redistribute it and'or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Ryu Changwoo <cwryu@eve.kaist.ac.kr>. */

#include <config.h>
#include <gnome.h>
#include <locale.h>

#include <getopt.h>
#include <long-options.h>

#include "io-gtk.h"
#include "option.h"
#include "score.h"

static void parse_args (int argc, char *argv[]);

/* The name this program invoked with.  */
char *program_name;

/* The entry of this program.  */
int
main (int argc, char **argv)
{
  program_name = argv[0];

  gnome_init(&argc, &argv);
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);

  parse_args(argc, argv);

  option_init();
  score_init();

  io_gtk_init ();
  io_gtk_loop ();
  return 0;
}


static void
parse_args (int argc, char *argv[])
{
  gint ch;

  struct option options[] =
  {
    /* Default args */
    { "help", 			no_argument,            NULL,   'h'     },
    { "version",	 	no_argument,            NULL,   'v'     },
    { NULL, 0, NULL, 0 }
  };

  /* initialize getopt */
  optarg = NULL;
  optind = 0;
  optopt = 0;

  while( (ch = getopt_long(argc, argv, "hv", options, NULL)) != EOF )
  {
    switch(ch)
    {
      case 'h':
        g_print ( 
      	  _("%s: FreeCell solitaire card game\n\n"
      	    "Usage: %s [--help] [--version]\n\n"
      	    "Options:\n"
      	    "        --help     display this help and exit\n"
      	    "        --version  output version information and exit\n"),
      	    argv[0], argv[0]);
        exit(0);
        break;
      case 'v':
        g_print (_("FreeCell %s.\n"), VERSION);
        exit(0);
        break;
      case ':':
      case '?':
        g_print (_("Options error\n"));
        exit(0);
        break;
    }
  }

  return;
}

