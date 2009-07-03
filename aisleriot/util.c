/* 
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#include <config.h>

#include <string.h>

#include <glib/gi18n.h>

#include <gtk/gtk.h>

#include <libgames-support/games-help.h>

#include "util.h"

static char *
game_file_to_help_section (const char *game_file)
{
  char *p, *buf;

  buf = g_path_get_basename (game_file);

  if ((p = strrchr (buf, '.')))
    *p = '\0';
  for (p = buf; p = strchr (p, '-'), p && *p;)
    *p = '_';
  for (p = buf; p = strchr (p, '_'), p && *p;) {
    char *next = p + 1;
    char q = *next;

    if (q != '\0' && g_ascii_islower (q)) {
      *next = g_ascii_toupper (q);
      ++p;
    }
  }
  if (g_ascii_islower (buf[0])) {
    buf[0] = g_ascii_toupper (buf[0]);
  }

  return buf;
}

void
aisleriot_display_help (GtkWindow *parent,
                        const char *game_file)
{
  char *help_section = NULL;

  if (game_file != NULL) {
    help_section = game_file_to_help_section (game_file);
  }

  games_help_display (GTK_WIDGET (parent), DOC_MODULE, help_section);
  g_free (help_section);
}
