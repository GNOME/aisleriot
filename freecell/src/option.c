/* option.c
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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Ryu Changwoo <cwryu@eve.kaist.ac.kr>. */


#include <config.h>
#include <gnome.h>

int option_inform_invalid_move;

void
option_init (void)
{
  option_inform_invalid_move
    = gnome_config_get_int ("/Freecell/option/inform_invalid_move=1");
}


void
option_write (void)
{
  gnome_config_set_int ("/Freecell/option/inform_invalid_move",
			option_inform_invalid_move);
  gnome_config_sync();
}


