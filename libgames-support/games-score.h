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

#ifndef GAMES_SCORE_H
#define GAMES_SCORE_H

#include <glib.h>
#include <time.h>

G_BEGIN_DECLS

/* GamesScore and GamesScoresStyle should be kept in sync. */
typedef enum {
  GAMES_SCORES_STYLE_PLAIN_DESCENDING,
  GAMES_SCORES_STYLE_PLAIN_ASCENDING,
  GAMES_SCORES_STYLE_TIME_DESCENDING,
  GAMES_SCORES_STYLE_TIME_ASCENDING,
} GamesScoreStyle;

typedef union {
  guint32 plain;
  gdouble time_double;		/* minutes.seconds */
} GamesScoreValue;

typedef struct {
  GamesScoreValue value;
  time_t time;
  gchar *name;
} GamesScore;

GamesScore *games_score_new (void);
GamesScore *games_score_dup (GamesScore * orig);
gint games_score_compare (GamesScoreStyle style, GamesScore * a,
			  GamesScore * b);
gint games_score_compare_values (GamesScoreStyle style, GamesScoreValue a,
				 GamesScoreValue b);
void games_score_destroy (GamesScore * score);

G_END_DECLS

#endif /* GAMES_SCORE_H */
