/* 
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
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

#include <string.h>

#include <glib/gi18n.h>

#include <gtk/gtk.h>

#include "ar-help.h"
#include "ar-show.h"
#include "ar-string-utils.h"

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

/**
 * aisleriot_show_help:
 * @window: a parent window to use for error messages
 * @game_file: the game to show help for, or %NULL to show
 *   general help
 *
 * Shows help for @game_file, or the main help if @game_file is %NULL.
 */
void
aisleriot_show_help (GtkWidget *window,
                        const char *game_file)
{
  char *help_section = NULL;
  GError *error = NULL;

  if (game_file != NULL) {
    help_section = game_file_to_help_section (game_file);
  }

  if (!ar_help_display_full (GTK_WIDGET (window), DOC_MODULE, help_section, &error)) {
    if (game_file != NULL) {
      char *help_section_display;

      help_section_display = ar_filename_to_display_name (game_file);

      ar_show_error (window, error,
                        _("Could not show help for “%s”"),
                        help_section_display);
    } else {
      ar_show_error (window, error,
                        _("Could not show help for “%s”"),
                        g_get_application_name ());
    }

    g_error_free (error);
  }

  g_free (help_section);
}

/**
 * aisleriot_variation_to_game_file:
 * @variation: name of a game from command line
 *
 * Creates a game file name from a command line --variation argument.
 * This strips dangerous characters like .. and /.
 *
 * Returns: a newly allocated string containing the game file name for @variation
 */
char *
aisleriot_variation_to_game_file (const char *variation)
{
  char *game_file, *s;

  game_file = g_ascii_strdown (variation, -1);

  /* Replace dangerous characters: '.' (as in ".."), '/' and '\' */
  g_strdelimit (game_file, "./\\" , '\0');
  g_strdelimit (game_file, NULL, '_');

  if (game_file[0] == '\0') {
    g_free (game_file);
    return NULL;
  }

  /* Add the suffix */
  s = g_strconcat (game_file, ".scm", NULL);
  g_free (game_file);

  return s;
}

/**
 * ar_atk_util_add_atk_relation:
 * @widget:
 * @other:
 * @type:
 *
 * Adds an AtkRelation of type @type to @other into @widget's
 * AtkRelationSet.
 */
void
ar_atk_util_add_atk_relation (GtkWidget *widget,
                              GtkWidget *other,
                              AtkRelationType type)
{
  AtkRelationSet *set;
  AtkRelation *relation;
  AtkObject *object;

  object = gtk_widget_get_accessible (other);
  set = atk_object_ref_relation_set (gtk_widget_get_accessible (widget));
  relation = atk_relation_new (&object, 1, type);
  atk_relation_set_add (set, relation);
  g_object_unref (relation);
  g_object_unref (set);
}
