/* freecell.c --
   Copyright (C) 1998 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
   USA */

/* Written by Changwoo Ryu <cwryu@adam.kaist.ac.kr>. */

#include <config.h>
#include <gnome.h>
#include <libgnomeui/gnome-window-icon.h>

#include "io-gtk.h"
#include "option.h"
#include "score.h"

/* The entry of this program.  */
int
main (int argc, char **argv)
{
  gnome_score_init("freecell");

  bindtextdomain (PACKAGE, GNOMELOCALEDIR);
  textdomain (PACKAGE);

  gnome_init("freecell", VERSION, argc, argv);
  gnome_window_icon_set_default_from_file (GNOME_ICONDIR"/gnome-cardgame.png");
  option_init();
  score_init();

  io_gtk_init ();
  io_gtk_loop ();
  return 0;
}
