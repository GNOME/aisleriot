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

G_DEFINE_TYPE (GamesScore, games_score, G_TYPE_OBJECT)

/**
 * games_score_new:
 * 
 * Creates a new score object.
 * 
 * Return value: the new #GamesScore
 **/
GamesScore *
games_score_new (void)
{
  return g_object_new (GAMES_TYPE_SCORE, NULL);
}

/**
 * games_score_dup:
 * @orig: The score to duplicate
 * 
 * Duplicates a score object.
 * 
 * Return value: (transfer full): A copy of @orig.
 **/
GamesScore *
games_score_dup (GamesScore * orig)
{
  GamesScore *duplicate;

  duplicate = games_score_new ();
  duplicate->value = orig->value;
  duplicate->time = orig->time;
  g_free (duplicate->name);
  duplicate->name = g_strdup (orig->name);

  return duplicate;
}

gint
games_score_compare_values (GamesScoreStyle style,
                            GamesScoreValue a,
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

static void
games_score_finalize (GObject * object)
{
  GamesScore *score = GAMES_SCORE (object);

  g_free (score->name);

  G_OBJECT_CLASS (games_score_parent_class)->finalize (object);
}

static void
games_score_class_init (GamesScoreClass * klass)
{
  GObjectClass *object_class = (GObjectClass *) klass;

  object_class->finalize = games_score_finalize;
}

static void
games_score_init (GamesScore *score)
{
  const gchar* name;

  score->time = time (NULL);
  /* FIXME: We don't handle the "Unknown" case. */
  name = g_get_real_name ();
  if (name[0] == '\0' || g_utf8_validate (name, -1, NULL) != TRUE) {
    name = g_get_user_name ();
    if (g_utf8_validate (name, -1, NULL) != TRUE) {
      name = "";
    }
  }
  score->name = g_strdup (name);
}
