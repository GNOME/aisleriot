/* score.c --
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

#include <sys/param.h>
#include <stdio.h>
#include <string.h>

#include <gnome.h>

static int win_number;
static int lose_number;
static int this_session_win_number;
static int this_session_lose_number;
static int streaks_win_number;
static int streaks_lose_number;

static int streaks_current;

#define FORMATSTRING_LINENUM 20
#define LINEMAX 1024  /* FIXME: is it enough ? */
static char formatstring[FORMATSTRING_LINENUM][LINEMAX]; 

  
int
score_formatstring (char **strings)
{
  int percentage;
  int n = 0;
  int i;

  if ((this_session_win_number + this_session_lose_number) == 0)
    percentage = 0;
  else
    percentage = (this_session_win_number * 100)
      / (this_session_win_number + this_session_lose_number);

  sprintf (formatstring[n++], _("This session: %d%%"), percentage);
  sprintf (formatstring[n++], _("won: %d, lost: %d"),
	   this_session_win_number, this_session_lose_number);
  sprintf (formatstring[n++], "    ");

  if ((win_number + lose_number) == 0)
    percentage = 0;
  else
    percentage = (win_number * 100) / (win_number + lose_number);
  
  sprintf (formatstring[n++], _("Total: %d%%"), percentage);
  sprintf (formatstring[n++], _("won: %d, lost: %d"), win_number, lose_number);
  sprintf (formatstring[n++], "    ");

  sprintf (formatstring[n++], _("Streaks"));
  sprintf (formatstring[n++], _("wins: %d, losses: %d"),
	   streaks_win_number, streaks_lose_number);

  if (streaks_current >= 0)
    sprintf (formatstring[n++], _("current: %d wins"), streaks_current);
  else
    sprintf (formatstring[n++], _("current: %d losses"), - streaks_current);

  if (strings)
    {
      for (i = 0; i < n; i++)
	strings[i] = formatstring[i];
      strings[n] = NULL;
    }
  
  return n;
}


void
score_init(void)
{
  win_number = gnome_config_get_int ("/freecell/score/win_number=0");
  lose_number = gnome_config_get_int ("/freecell/score/lose_number=0");
  streaks_win_number = gnome_config_get_int ("/freecell/score/streaks_win_number=0");
  streaks_lose_number = gnome_config_get_int
    ("/freecell/score/streaks_lose_number=0");
  streaks_current = gnome_config_get_int
    ("/freecell/score/streaks_current=0");

  this_session_win_number = 0;
  this_session_lose_number = 0;
}

void
score_write(void)
{
  gnome_config_set_int ("/freecell/score/win_number",
			win_number);
  gnome_config_set_int ("/freecell/score/lose_number",
			lose_number);
  gnome_config_set_int ("/freecell/score/streaks_win_number",
			streaks_win_number);
  gnome_config_set_int ("/freecell/score/streaks_lose_number",
			streaks_lose_number);
  gnome_config_set_int ("/freecell/score/streaks_current",
			streaks_current);
  gnome_config_sync();
}

void
score_clear(void)
{
  win_number = 0;
  lose_number = 0;
  this_session_win_number = 0;
  this_session_lose_number = 0;
  streaks_win_number = 0;
  streaks_lose_number = 0;
  streaks_current = 0;
}


void
score_add_win(void)
{
  win_number++;
  this_session_win_number++;

  if (streaks_current == 0)
    streaks_current = 1;
  else if (streaks_current > 0)
    streaks_current++;
  else
    streaks_current = 1;

  if (streaks_current > streaks_win_number)
    {
      streaks_win_number = streaks_current;
    }
}

  
void
score_add_lose(void)
{
  lose_number++;
  this_session_lose_number++;

  if (streaks_current == 0)
    streaks_current = -1;
  else if (streaks_current < 0)
    streaks_current--;
  else
    streaks_current = -1;

  if (-streaks_current > streaks_lose_number)
    {
      streaks_lose_number = - streaks_current;
    }
}


      



