/* card-style-files.c
   Copyright 2003 Free Software Foundation, Inc.

   This library is free software; you can redistribute it and'or modify
   it under the terms of the GNU Library General Public License as published 
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  

   Author: Callum McKenzie

   This file contains the routines for parsing a file of card styles.
*/

#include <glib.h>
#include <string.h> /* For memmove */

#include "card-style-file.h"

gint card_style_compare (gconstpointer a, gconstpointer b)
{
  return g_utf8_collate (((const CardDeckStyle *)a)->name, 
			 ((const CardDeckStyle *)b)->name);
}


typedef struct _CardStyleFileContext {
  GList * style_list;
  gchar ** current_string;
} CardStyleFileContext;

static void
card_style_file_parse_start (GMarkupParseContext *context,
			     const gchar *element,
			     const gchar **attribute_names,
			     const gchar **attribute_values,
			     gpointer data,
			     GError **error)
{
  CardDeckStyle * style;
  int i;
  gchar * element_names[] = {"back", "honor", "joker", "rankfont",
			     "smallfont", "mediumfont", "largefont" };
  CardStyleFileContext * ctxt = (CardStyleFileContext *)data;

  if (g_utf8_collate ("cardstyle", element) == 0) {
    style = g_malloc0 (sizeof (CardDeckStyle));
    ctxt->style_list = g_list_prepend (ctxt->style_list, style);
    return;
  }

  if ((ctxt->style_list == NULL) ||
      (ctxt->style_list->data == NULL))
    return;

  style = (CardDeckStyle *)(ctxt->style_list->data);

  if (g_utf8_collate ("name", element) == 0) {
    ctxt->current_string = &(style->name);
    return;
  }

  for (i = 0; i<NUM_COMPONENTS; i++) {
    if (g_utf8_collate (element_names[i], element) == 0) {
      ctxt->current_string = &(style->components[i]);
      return;
    }
  }
}

static void
card_style_file_parse_stop (GMarkupParseContext * context,
			    const gchar * element,
			    gpointer data,
			    GError ** error)
{
  CardStyleFileContext * ctxt = (CardStyleFileContext *)data;

  /* Ignore text that is not enclosed in the right tags. */
  ctxt->current_string = NULL;
}

static void
card_style_file_parse_text (GMarkupParseContext * context,
			    const gchar * text,
			    gsize length,
			    gpointer data,
			    GError ** error)
{
  CardStyleFileContext * ctxt = (CardStyleFileContext *)data;

  if (ctxt->current_string == NULL)
    return;

  *(ctxt->current_string) = g_malloc (length + 1);
  g_memmove (*(ctxt->current_string), text, length);
  *(*(ctxt->current_string)+length) = '\0';
}


GList * 
card_style_file_parse (gchar * filename)
{
  GMarkupParser parser = { card_style_file_parse_start, 
			   card_style_file_parse_stop, 
			   card_style_file_parse_text, 
			   NULL, NULL };
  GMarkupParseContext * parse_context;
  gchar * file;
  gint length;
  gboolean ok;
  CardStyleFileContext * ctxt;

  ctxt = g_malloc (sizeof (CardStyleFileContext));
  ctxt->style_list = NULL;
  ctxt->current_string = NULL;

  ok = g_file_get_contents (filename, &file, &length, NULL);
  if (!ok)
    return NULL;

  parse_context = g_markup_parse_context_new (&parser, 0, ctxt, 
					      NULL);
  g_markup_parse_context_parse (parse_context, file, length, NULL);
  g_markup_parse_context_free (parse_context);

  g_free (file);

  return ctxt->style_list;
}
