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
#include <gconf/gconf-client.h>

gboolean option_inform_invalid_move = FALSE;
gboolean option_move_one_by_one = FALSE;

extern GConfClient *freecell_gconf_client;
gint
freecell_gconf_get_int (GConfClient *client, gchar *key, gint default_int);
gboolean
freecell_gconf_get_bool (GConfClient *cilent, gchar *key, gboolean default_bool);

void
option_init (void)
{
  option_inform_invalid_move = freecell_gconf_get_bool (freecell_gconf_client,
			"/apps/freecell/option/inform_invalid_move", TRUE);
  option_move_one_by_one = freecell_gconf_get_bool (freecell_gconf_client,
			"/apps/freecell/option/move_one_by_one", FALSE);
}


void
option_write (void)
{
  gconf_client_set_bool (freecell_gconf_client,
			"/apps/freecell/option/inform_invalid_move",
			option_inform_invalid_move, NULL);
  gconf_client_set_bool (freecell_gconf_client,
			"/apps/freecell/option/move_one_by_one",
			option_move_one_by_one, NULL);
}


gint
freecell_gconf_get_int (GConfClient *client, gchar *key, gint default_int)
{
  GConfValue *value = NULL;
  GConfValue *schema_value = NULL;
  gint retval;

  value = gconf_client_get (client, key, NULL);
  if (value == NULL)
    return default_int;

  if (value->type == GCONF_VALUE_INT) {
    retval = gconf_value_get_int (value);
    gconf_value_free (value);
  }
  else {
    schema_value = gconf_client_get_default_from_schema (client, key, NULL);
    if (schema_value == NULL) {
      retval = default_int;
    }
    else {
      retval = gconf_value_get_int (schema_value);
    }
    gconf_value_free (value);
    gconf_value_free (schema_value);
  }

  return retval;
}

gboolean
freecell_gconf_get_bool (GConfClient *client, gchar *key, gboolean default_bool)
{
  GConfValue *value = NULL;
  GConfValue *schema_value = NULL;
  gboolean retval;

  value = gconf_client_get (client, key, NULL);
  if (value == NULL)
    return default_bool;

  if (value->type == GCONF_VALUE_BOOL) {
    retval = gconf_value_get_bool (value);
    gconf_value_free (value);
  }
  else {
    schema_value = gconf_client_get_default_from_schema (client, key, NULL);
    if (schema_value == NULL) {
      retval = default_bool;
    }
    else {
      retval = gconf_value_get_bool (schema_value);
    }
    gconf_value_free (value);
    gconf_value_free (schema_value);
  }

  return retval;
}

