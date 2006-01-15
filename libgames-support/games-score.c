/* games-score.c 
 *
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

/* We don't make it a proper object, basically to reduce overhead (system
   memory and programmers time) */

/* FIXME: Should memory for these be allocated as a block? (see GMemChunk 
   and GAllocator)*/

#include "games-score.h"

GamesScore *games_score_new (void)
{
  GamesScore *newscore;

  newscore = g_new0 (GamesScore, 1);
  newscore->time = time (NULL);
  /* FIXME: We don't handle the "Unknown" case. */
  newscore->name = g_strdup (g_get_real_name ());

  return newscore;
}

GamesScore *games_score_dup (GamesScore *orig)
{
  GamesScore *new;

  new = games_score_new ();
  
  /* FIXME: What is the canonical way to duplicate a union?
   * Can we just use the largest item or is it more subtle than
   * that. */
  new->value.plain = orig->value.plain;
  new->value.time_double = orig->value.time_double;

  new->time = orig->time;
  new->name = g_strdup (orig->name);

  return new;
}

void games_score_destroy (GamesScore *score)
{
  if (score->name)
    g_free (score->name);

  g_free (score);
}

gint games_score_compare_values (GamesScoreStyle style, GamesScoreValue a, 
				 GamesScoreValue b)
{
  switch (style) {
  case GAMES_SCORES_STYLE_PLAIN_DESCENDING:
    if (a.plain > b.plain) return +1;
    if (a.plain < b.plain) return -1;
    return 0;
  case GAMES_SCORES_STYLE_PLAIN_ASCENDING:
    if (a.plain > b.plain) return -1;
    if (a.plain < b.plain) return +1;
    return 0;
  case GAMES_SCORES_STYLE_TIME_DESCENDING:
    if (a.time_double > b.time_double) return +1;
    if (a.time_double < b.time_double) return -1;
    return 0;
  case GAMES_SCORES_STYLE_TIME_ASCENDING:
    if (a.time_double > b.time_double) return -1;
    if (a.time_double < b.time_double) return +1;
    return 0;
  default:
    g_warning ("Uknown score style in games_score_compare - treating as equal.");
    return 0;
  }
}

gint games_score_compare (GamesScoreStyle style, GamesScore *a, GamesScore *b)
{
  return games_score_compare_values (style, a->value, b->value);
}
