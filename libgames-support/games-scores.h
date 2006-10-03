/* Games Scores Dialog - Display high scores
 *
 * Copyright (c) 2005 by Callum McKenzie
 * This library is free software; you can redistribute it and'or modify
 * it under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
 *
 */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#include <glib.h>
#include <glib-object.h>

#ifndef GAMES_SCORES_H
#define GAMES_SCORES_H

G_BEGIN_DECLS
#include "games-score.h"
#include "games-scores-backend.h"
/* How many scores get counted as significant. */
#define GAMES_SCORES_SIGNIFICANT 10
  typedef struct {
  gchar *key;			/* A unique identifier (warning: this is used to generate the
				 * scores file name, so it should match the old domains) */
  gchar *name;			/* A human-readable description. */
} GamesScoresCategory;

typedef void (*GamesScoresCategoryForeachFunc) (GamesScoresCategory * cat,
						gpointer data);

#define GAMES_SCORES_LAST_CATEGORY {NULL, NULL}

/* All elements get copied so the creator stays the owner. */
typedef struct {
  const GamesScoresCategory *categories;	/* Array of categories, terminate
						 * with GAMES_SCORES_LAST_CATEGORY. */
  gchar *deflt;			/* The key of the default category. */
  gchar *basename;		/* The base of the filename. The old appname. */
  GamesScoreStyle style;
} GamesScoresDescription;

#define GAMES_TYPE_SCORES (games_scores_get_type())
#define GAMES_SCORES(obj) G_TYPE_CHECK_INSTANCE_CAST((obj), games_scores_get_type(), GamesScores)
#define GAMES_SCORES_CONST(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), games_scores_get_type(), GamesScores const)
#define GAMES_SCORES_CLASS(klass) G_TYPE_CHECK_CLASS_CAST((klass), games_scores_get_type(), GamesScoresClass)
#define GAMES_IS_SCORES(obj) G_TYPE_CHECK_INSTANCE_TYPE((obj), games_scores_get_type ())

#define GAMES_SCORES_GET_CLASS(obj) G_TYPE_INSTANCE_GET_CLASS((obj), games_scores_get_type(), GamesScoresClass)

typedef struct _GamesScores {
  GObject parent;
  struct _GamesScoresPrivate *priv;
} GamesScores;

typedef struct _GamesScoresClass {
  GObjectClass parent;
} GamesScoresClass;

GType games_scores_get_type (void);
GamesScores *games_scores_new (const GamesScoresDescription * description);
void games_scores_set_category (GamesScores * self, gchar * category);
gint games_scores_add_score (GamesScores * self, GamesScoreValue score);
void games_scores_update_score (GamesScores * self, gchar * new_name);
GList *games_scores_get (GamesScores * self);
void games_scores_category_foreach (GamesScores * self,
				    GamesScoresCategoryForeachFunc func,
				    gpointer userdata);
GamesScoreStyle games_scores_get_style (GamesScores * self);
const gchar *games_scores_get_category (GamesScores * self);
void games_scores_add_category (GamesScores * self, gchar * key,
				gchar * name);

G_END_DECLS
#endif /* GAMES_SCORES_H */
