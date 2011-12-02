/*
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
 *
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

#include "ar-string-utils.h"

/**
 * ar_filename_to_display_name:
 * @filename:
 *
 * Transforms @filename from filename encoding into a
 * translated string in UTF-8 that can be shown to the
 * user.
 *
 * Returns: a newly allocated UTF-8 string
 */
char *
ar_filename_to_display_name (const char *filename)
{
  char *base_name, *display_name, *translated, *p;
  GString *prettified_name;
  gboolean start_of_word, free_segment;
  gunichar c;
  char utf8[7];
  gsize len;

  g_return_val_if_fail (filename != NULL, NULL);

  base_name = g_path_get_basename (filename);
  g_return_val_if_fail (base_name != NULL, NULL);

  /* Hide extension */
  g_strdelimit (base_name, ".", '\0');
  /* Hide undesirable characters */
  g_strdelimit (base_name, NULL, ' ');

  g_strstrip (base_name);

  display_name = g_filename_display_name (base_name);
  g_free (base_name);

  g_return_val_if_fail (display_name != NULL, NULL);

  /* Now turn the first character in each word to uppercase */

  prettified_name = g_string_sized_new (strlen (display_name) + 8);
  start_of_word = TRUE;
  for (p = display_name; p && *p; p = g_utf8_next_char (p)) {
    if (start_of_word) {
      c = g_unichar_toupper (g_utf8_get_char (p));
    } else {
      c = g_utf8_get_char (p);
    }

    len = g_unichar_to_utf8 (c, utf8);
    g_string_append_len (prettified_name, utf8, len);

    start_of_word = g_unichar_isspace (c);
  }
  g_free (display_name);

  translated = gettext (prettified_name->str);
  if (translated != prettified_name->str) {
    display_name = g_strdup (translated);
    free_segment = TRUE;
  } else {
    display_name = prettified_name->str;
    free_segment = FALSE;
  }

  g_string_free (prettified_name, free_segment);

  return display_name;
}

/**
 * ar_filename_to_game_module:
 * @game_file: name of a game from command line
 *
 * Creates a game module name from a command line argument.
 *
 * Returns: a newly allocated string containing the game file name for @game_file
 */
char *
ar_filename_to_game_module (const char *game_file)
{
  char *game_module;

  game_module = g_ascii_strdown (game_file, -1);

  /* Replace dangerous characters: '.' (as in ".."), '/' and '\' */
  g_strdelimit (game_module, "./\\" , '\0');
  if (game_module[0] == '\0') {
    g_free (game_module);
    return NULL;
  }

  g_strdelimit (game_module, NULL, '-');
  return game_module;
}
