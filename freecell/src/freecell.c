/* freecell.c
   Copyright (C) 1997, 1998 Ryu Changwoo

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

#include "io-gtk.h"
#include "option.h"
#include "score.h"

/* The entry of this program.  */
int
main (int argc, char **argv)
{
  argp_program_version = VERSION;

  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);

  gnome_init("freecell", NULL, argc, argv, 0, NULL);

  option_init();
  score_init();

  io_gtk_init ();
  io_gtk_loop ();
  return 0;
}
