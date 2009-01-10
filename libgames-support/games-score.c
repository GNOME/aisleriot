/*
 * Copyright (C) 2005 Callum McKenzie
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>

#include "games-score.h"

GamesScore *
games_score_new (void)
{
  GamesScore *newscore;
  const gchar* name;

  newscore = g_slice_new0 (GamesScore);
  newscore->time = time (NULL);
  /* FIXME: We don't handle the "Unknown" case. */
  name = g_get_real_name ();
  if (name[0] == '\0' || g_utf8_validate (name, -1, NULL) != TRUE) {
    name = g_get_user_name ();
    if (g_utf8_validate (name, -1, NULL) != TRUE) {
      name = "";
    }
  }
  newscore->name = g_strdup (name);

  return newscore;
}

GamesScore *
games_score_dup (GamesScore * orig)
{
  GamesScore *new;

  new = g_slice_new (GamesScore);
  *new = *orig;
  new->name = g_strdup (orig->name);

  return new;
}

void
games_score_destroy (GamesScore * score)
{
  g_free (score->name);
  g_slice_free (GamesScore, score);
}

gint
games_score_compare_values (GamesScoreStyle style, GamesScoreValue a,
			    GamesScoreValue b)
{
  switch (style) {
  case GAMES_SCORES_STYLE_PLAIN_DESCENDING:
    if (a.plain > b.plain)
      return +1;
    if (a.plain < b.plain)
      return -1;
    return 0;
  case GAMES_SCORES_STYLE_PLAIN_ASCENDING:
    if (a.plain > b.plain)
      return -1;
    if (a.plain < b.plain)
      return +1;
    return 0;
  case GAMES_SCORES_STYLE_TIME_DESCENDING:
    if (a.time_double > b.time_double)
      return +1;
    if (a.time_double < b.time_double)
      return -1;
    return 0;
  case GAMES_SCORES_STYLE_TIME_ASCENDING:
    if (a.time_double > b.time_double)
      return -1;
    if (a.time_double < b.time_double)
      return +1;
    return 0;
  default:
    g_warning
      ("Uknown score style in games_score_compare - treating as equal.");
    return 0;
  }
}

gint
games_score_compare (GamesScoreStyle style, GamesScore * a, GamesScore * b)
{
  return games_score_compare_values (style, a->value, b->value);
}
