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

struct GamesScorePrivate {
  union {
    guint32 plain;
    gdouble time_double;		/* minutes.seconds */
  } value;
  time_t time;
  gchar *name;
};

/**
 * games_score_new:
 * 
 * Creates a new score object.
 * 
 * Return value: the new #GamesScore
 **/
GamesScore *
games_score_new ()
{
  return g_object_new (GAMES_TYPE_SCORE, NULL);
}

/**
 * games_score_new_plain:
 * @value: The value of the score.
 * 
 * Creates a new score object.
 * 
 * Return value: the new #GamesScore
 **/
GamesScore *
games_score_new_plain (guint32 value)
{
  GamesScore *score = g_object_new (GAMES_TYPE_SCORE, NULL);
  score->priv->value.plain = value;
  return score;
}

/**
 * games_score_new_time:
 * @value: The timer value of the score.
 * 
 * Creates a new score object.
 * 
 * Return value: the new #GamesScore
 **/
GamesScore *
games_score_new_time (gdouble value)
{
  GamesScore *score = g_object_new (GAMES_TYPE_SCORE, NULL);
  score->priv->value.time_double = value;
  return score;
}

const gchar *
games_score_get_name (GamesScore *score)
{
  return score->priv->name;
}

void
games_score_set_name (GamesScore *score, const gchar *name)
{
  g_free (score->priv->name);
  score->priv->name = g_strdup (name);
}

time_t
games_score_get_time (GamesScore *score)
{
  return score->priv->time;
}

void
games_score_set_time (GamesScore *score, time_t time)
{
  score->priv->time = time;  
}

guint32
games_score_get_value_as_plain (GamesScore *score)
{
  return score->priv->value.plain;
}

gdouble
games_score_get_value_as_time (GamesScore *score)
{
  return score->priv->value.time_double;
}

gint
games_score_compare (GamesScoreStyle style,
                     GamesScore * a,
                     GamesScore * b)
{
  switch (style) {
  case GAMES_SCORES_STYLE_PLAIN_DESCENDING:
    if (a->priv->value.plain > b->priv->value.plain)
      return +1;
    if (a->priv->value.plain < b->priv->value.plain)
      return -1;
    return 0;
  case GAMES_SCORES_STYLE_PLAIN_ASCENDING:
    if (a->priv->value.plain > b->priv->value.plain)
      return -1;
    if (a->priv->value.plain < b->priv->value.plain)
      return +1;
    return 0;
  case GAMES_SCORES_STYLE_TIME_DESCENDING:
    if (a->priv->value.time_double > b->priv->value.time_double)
      return +1;
    if (a->priv->value.time_double < b->priv->value.time_double)
      return -1;
    return 0;
  case GAMES_SCORES_STYLE_TIME_ASCENDING:
    if (a->priv->value.time_double > b->priv->value.time_double)
      return -1;
    if (a->priv->value.time_double < b->priv->value.time_double)
      return +1;
    return 0;
  default:
    g_warning
      ("Uknown score style in games_score_compare - treating as equal.");
    return 0;
  }
}

static void
games_score_finalize (GObject * object)
{
  GamesScore *score = GAMES_SCORE (object);

  g_free (score->priv->name);

  G_OBJECT_CLASS (games_score_parent_class)->finalize (object);
}

static void
games_score_class_init (GamesScoreClass * klass)
{
  GObjectClass *object_class = (GObjectClass *) klass;

  object_class->finalize = games_score_finalize;

  g_type_class_add_private (object_class, sizeof (GamesScorePrivate));
}

static void
games_score_init (GamesScore *score)
{
  const gchar* name;

  score->priv = G_TYPE_INSTANCE_GET_PRIVATE (score, GAMES_TYPE_SCORE, GamesScorePrivate);

  score->priv->time = time (NULL);
  /* FIXME: We don't handle the "Unknown" case. */
  name = g_get_real_name ();
  if (name[0] == '\0' || g_utf8_validate (name, -1, NULL) != TRUE) {
    name = g_get_user_name ();
    if (g_utf8_validate (name, -1, NULL) != TRUE) {
      name = "";
    }
  }
  score->priv->name = g_strdup (name);
}
