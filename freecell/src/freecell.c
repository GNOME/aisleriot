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
#include <gconf/gconf-client.h>

#include "io-gtk.h"
#include "option.h"
#include "score.h"

GConfClient *freecell_gconf_client = NULL;

void key_changed_callback (GConfClient *tmp_client, guint cnx_id,
                           GConfEntry *tmp_entry, gpointer tmp_data)
{
  option_init ();
}

/* The entry of this program.  */
int
main (int argc, char **argv)
{
  gnome_score_init("freecell");

  bindtextdomain (GETTEXT_PACKAGE, GNOMELOCALEDIR);
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  textdomain (GETTEXT_PACKAGE);

  gnome_program_init ("freecell", VERSION,
		  LIBGNOMEUI_MODULE,
		  argc, argv,
		      GNOME_PARAM_POPT_TABLE, NULL,
		      GNOME_PARAM_APP_DATADIR, DATADIR, NULL);

  freecell_gconf_client = gconf_client_get_default ();
  gconf_client_add_dir (freecell_gconf_client, "/apps/freecell/option",
    GCONF_CLIENT_PRELOAD_NONE, NULL);
  gconf_client_notify_add (freecell_gconf_client, "/apps/freecell/option",
    key_changed_callback, NULL, NULL, NULL);

  gnome_window_icon_set_default_from_file (GNOME_ICONDIR"/gnome-cardgame.png");
  option_init();
  score_init();

  io_gtk_init ();
  io_gtk_loop ();

  g_object_unref (G_OBJECT (freecell_gconf_client));

  return 0;
}
