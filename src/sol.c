/*
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
 * Copyright © 2007 Andreas Røsdal <andreasr@gnome.org> 
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <libguile.h>

#include <glib.h>
#include <glib/gi18n.h>

#include "ar-runtime.h"

#include "application.h"

static void
main_prog (void *closure, int argc, char *argv[])
{
  GtkApplication *application;
  int status;

  application = aisleriot_application_new ();
  status = g_application_run (G_APPLICATION (application), argc, argv);
  g_object_unref (application);
}

int
main (int argc, char *argv[])
{
  if (!ar_runtime_init ("aisleriot"))
    return 1;

  g_setenv ("GUILE_WARN_DEPRECATED", "detailed", TRUE);
  g_setenv ("GUILE_AUTO_COMPILE", "0", TRUE);

  g_setenv ("UBUNTU_MENUPROXY", "0", TRUE);

  scm_boot_guile (argc, argv, main_prog, NULL); /* no return */

  ar_runtime_shutdown ();

  return 0;
}
