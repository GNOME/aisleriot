/* games-find-file.c:

   Copyright 2006 Callum McKenzie

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Authors:   Callum McKenzie <callum@spooky-possum.org> */

#include <config.h>

#include <glib.h>

#include "games-files.h"
#include "games-find-file.h"

static gchar *
make_canonical_name (const gchar * name)
{
  gchar *cname;
  gchar *s;

  /* Strip the path. */
  cname = g_path_get_basename (name);
  /* Strip the suffix. */
  s = g_strrstr (cname, ".");
  if (s)
    *s = '\0';
  /* Remove case-sensitivity. */
  s = g_utf8_casefold (cname, -1);
  g_free (cname);
  cname = s;
  /* Normalise the UTF-8 encoding. */
  s = g_utf8_normalize (cname, -1, G_NORMALIZE_ALL);
  g_free (cname);
  cname = s;

  return cname;
}

static gint
compare_names (const gchar * filename, const gchar * ctarget)
{
  gchar *cname;
  gint keepgoing;

  cname = make_canonical_name (filename);

  keepgoing = g_utf8_collate (cname, ctarget);

  g_free (cname);

  return keepgoing;
}

gchar *
games_find_similar_file (const gchar * target, const gchar * directory)
{
  GamesFileList *list;
  gchar *result;
  gchar *ctarget;

  ctarget = make_canonical_name (target);

  list = games_file_list_new ("*", directory, NULL);

  result = games_file_list_find (list, (GCompareFunc) compare_names, ctarget);

  g_object_unref (list);
  g_free (ctarget);

  return result;
}
