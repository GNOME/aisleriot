/* option.c --
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

int option_inform_invalid_move = 0;
int option_move_one_by_one = 0;

void
option_init (void)
{
  option_inform_invalid_move
    = gnome_config_get_int ("/freecell/option/inform_invalid_move=1");
  option_move_one_by_one
    = gnome_config_get_int ("/freecell/option/move_one_by_one=0");
}


void
option_write (void)
{
  gnome_config_set_int ("/freecell/option/inform_invalid_move",
			option_inform_invalid_move);
  gnome_config_set_int ("/freecell/option/move_one_by_one",
			option_move_one_by_one);
  gnome_config_sync();
}


