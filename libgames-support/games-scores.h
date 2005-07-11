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
#include <gtk/gtk.h>

#ifndef GAMES_SCORES_H
#define GAMES_SCORES_H

G_BEGIN_DECLS

#include "games-score.h"
#include "games-scores-backend.h"

typedef struct {
  gchar *key;  /* A unique identifier (warning: this is used to generate the
		* scores file name, so it should match the old domains) */
  gchar *name; /* A human-readable description. */
} GamesScoresCategory;

/* All elements get copied so the creator stays the owner. */
typedef struct {
  GamesScoresCategory ** categories; /* NULL terminated! */
  gchar * deflt; /* The key of the default category. */
  gchar * filename;
  GamesScoreStyle style;
} GamesScoresDescription;

#define GAMES_TYPE_SCORES (games_scores_get_type())
#define GAMES_SCORES(obj) G_TYPE_CHECK_INSTANCE_CAST((obj), games_scores_get_type(), GamesScores)
#define GAMES_SCORES_CONST(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), games_scores_get_type(), GamesScores const)
#define GAMES_SCORES_CLASS(klass) G_TYPE_CHECK_CLASS_CAST((klass), games_scores_get_type(), GamesScoresClass)
#define GAMES_IS_SCORES(obj) G_TYPE_CHECK_INSTANCE_TYPE((obj), games_scores_get_type ())

#define GAMES_SCORES_GET_CLASS(obj) G_TYPE_INSTANCE_GET_CLASS((obj), games_scores_get_type(), GamesScoresClass)

typedef struct _GamesScoresPrivate {
  GHashTable * categories;
  gchar * currentcat;
  gchar * defcat;
  gboolean last_score_significant;
  gint last_score_position;
  GamesScoreStyle style;
  GamesScoresBackend *backend;
  GtkWidget * dialog;
} GamesScoresPrivate;

typedef struct _GamesScores {
	GObject parent;
	GamesScoresPrivate *priv;
} GamesScores;

typedef struct _GamesScoresClass {
	GObjectClass parent;
} GamesScoresClass;

GType games_scores_get_type (void);
GObject *games_scores_new (GamesScoresDescription * description);
void games_scores_set_category (GamesScores * self, gchar * category);
gboolean games_scores_add_score	(GamesScores * self, GamesScore score);
void games_scores_show (GamesScores * self, gboolean hilight);

G_END_DECLS

#endif /* GAMES_SCORES_H */
