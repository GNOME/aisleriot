 /* games-scores-backend.c 
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

#ifndef GAMES_SCORES_BACKEND_H
#define GAMES_SCORES_BACKEND_H

#include <glib.h>
#include <glib-object.h>

#include <time.h>

#include "games-setgid-io.h"
#include "games-score.h"

G_BEGIN_DECLS

#define GAMES_TYPE_SCORES_BACKEND (games_scores_backend_get_type ())
#define GAMES_SCORES_BACKEND(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_SCORES_BACKEND, GamesScoresBackend))
#define GAMES_SCORES_BACKEND_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_SCORES_BACKEND, GamesScoresBackendClass))
#define GAMES_IS_SCORES_BACKEND(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_SCORES_BACKEND))
#define GAMES_IS_SCORES_BACKEND_CLASS(kls) (G_TYPE_CHECK_CLASS_TYPE ((kls), GAMES_TYPE_SCORES_BACKEND))
#define GAMES_GET_SCORES_BACKEND_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_SCORES_BACKEND, GamesScoresBackendClass))

typedef struct _GamesScoresBackend        GamesScoresBackend;
typedef struct _GamesScoresBackendPrivate GamesScoresBackendPrivate;
typedef struct _GamesScoresBackendClass   GamesScoresBackendClass;

struct _GamesScoresBackend {
  GObject object;
  GList *scores_list;
  GamesScoresBackendPrivate *priv;
};

struct _GamesScoresBackendClass {
  GObjectClass parent_class;
};

GType games_scores_backend_get_type (void);
GamesScoresBackend *games_scores_backend_new (GamesScoreStyle style,
					      char *base_name,
                                              char *name);
GList *games_scores_backend_get_scores (GamesScoresBackend * self);
gboolean games_scores_backend_set_scores (GamesScoresBackend * self,
				          GList * list);
void games_scores_backend_discard_scores (GamesScoresBackend * self);

G_END_DECLS
#endif /* GAMES_SCORES_BACKEND_H */
