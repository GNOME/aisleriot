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

#include <glib-object.h>
#include <time.h>

G_BEGIN_DECLS

#define GAMES_TYPE_SCORE            (games_score_get_type ())
#define GAMES_SCORE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_SCORE, GamesScore))
#define GAMES_SCORE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_SCORE, GamesScoreClass))
#define GAMES_IS_SCORE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_SCORE))
#define GAMES_IS_SCORE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_SCORE))

typedef struct GamesScorePrivate GamesScorePrivate;

/* GamesScore and GamesScoresStyle should be kept in sync. */
typedef enum {
  GAMES_SCORES_STYLE_PLAIN_DESCENDING,
  GAMES_SCORES_STYLE_PLAIN_ASCENDING,
  GAMES_SCORES_STYLE_TIME_DESCENDING,
  GAMES_SCORES_STYLE_TIME_ASCENDING,
} GamesScoreStyle;

typedef struct {
  GObject parent;
  /*< private >*/
  GamesScorePrivate *priv;
} GamesScore;

typedef struct {
  GObjectClass parent_class;
} GamesScoreClass;

GType        games_score_get_type           (void);
GamesScore  *games_score_new                (void);
GamesScore  *games_score_new_plain          (guint32 value);
GamesScore  *games_score_new_time           (gdouble value);
const gchar *games_score_get_name           (GamesScore *score);
void         games_score_set_name           (GamesScore *score, const gchar *name);
time_t       games_score_get_time           (GamesScore *score);
void         games_score_set_time           (GamesScore *score, time_t time);
guint32      games_score_get_value_as_plain (GamesScore *score);
gdouble      games_score_get_value_as_time  (GamesScore *score);
gint         games_score_compare            (GamesScoreStyle style,
                                             GamesScore * a,
                                             GamesScore * b);

G_END_DECLS

#endif /* GAMES_SCORE_H */
