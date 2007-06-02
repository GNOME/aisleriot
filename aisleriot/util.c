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

#include "config.h"

#include <string.h>

#include <glib/gi18n.h>

#include <gtk/gtk.h>

#include "util.h"

static AisleriotHelpFunc help_hook;
static gpointer help_hook_data;

/**
 * aisleriot_util_get_display_filename:
 * @filename:
 *
 * Transforms @filename from filename encoding into a
 * translated string in UTF-8 that can be shown to the
 * user.
 *
 * Returns: a newly allocated UTF-8 string
 */
char *
aisleriot_util_get_display_filename (const char *filename)
{
  char *basename, *display_name, *translated, *p;
  GString *prettified_name;
  gboolean start_of_word, free_segment;
  gunichar c;
  char utf8[7];
  gsize len;

  g_return_val_if_fail (filename != NULL, NULL);

  basename = g_path_get_basename (filename);
  g_return_val_if_fail (basename != NULL, NULL);

  /* Hide extension */
  g_strdelimit (basename, ".", '\0');
  /* Hide undesirable characters */
  g_strdelimit (basename, NULL, ' ');

  g_strstrip (basename);

  display_name = g_filename_display_name (basename);
  g_free (basename);

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

void
aisleriot_util_set_help_func (AisleriotHelpFunc func,
                              gpointer user_data)
{
  help_hook = func;
  help_hook_data = user_data;
}

void
aisleriot_display_help (GtkWindow *parent,
                        const char *game_file)
{
  if (!help_hook)
    return;

  help_hook (parent, game_file, help_hook_data);
}
