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

#include <glib.h>
#include <glib-object.h>

#include <fcntl.h>
#include <unistd.h>

#include "games-score.h"
#include "games-scores-backend.h"

G_DEFINE_TYPE (GamesScoresBackend, games_scores_backend, G_TYPE_OBJECT);

static void
games_scores_backend_finalize (GamesScoresBackend *backend)
{
  if (backend->priv->fileok) {
    close (backend->priv->fd);
  }
}

static void
games_scores_backend_class_init (GamesScoresBackendClass *klass)
{
  GObjectClass *oclass = G_OBJECT_CLASS (klass);

  g_type_class_add_private (klass, sizeof (GamesScoresBackendPrivate));
  oclass->finalize = (GObjectFinalizeFunc) games_scores_backend_finalize;
}

static void
games_scores_backend_init (GamesScoresBackend *backend)
{
  backend->priv = G_TYPE_INSTANCE_GET_PRIVATE (backend, 
					       GAMES_TYPE_SCORES_BACKEND,
					       GamesScoresBackendPrivate);
}

GamesScoresBackend *games_scores_backend_new (GamesScoreStyle style, 
					      gchar *name)
{
  GamesScoresBackend *backend;

  backend = GAMES_SCORES_BACKEND (g_object_new (GAMES_TYPE_SCORES_BACKEND, 
						NULL));

  backend->priv->style = style;
  backend->scores_list = NULL;
  backend->priv->fileok = FALSE;
  backend->priv->filename = g_build_filename (SCORESDIR, name, NULL);
  backend->priv->fd = open (backend->priv->filename, O_RDWR);
  if (backend->priv->fd != 0)
    backend->priv->fileok = TRUE;

  return backend;
}

GList *games_scores_backend_get_list (GamesScoresBackend *self) 
{
  /* Check for a change in the scores file and update if necessary. */
  
  if ((self->scores_list == NULL) /* || something */ ) {
    /* Lock the file and get the list. */
  }
  
  return self->scores_list;
}

gint games_scores_backend_insert_score (GamesScoresBackend *self,
					GamesScore *score)
{
    GList *s;

    s = self->scores_list;

    while (s != NULL) {
      GamesScore *oldscore = s->data;

      /* FIXME: Lock the file and check that the list is up to date. */

      /* FIXME: This is all hinting that a games_score should also be
       * a class. */
      if (games_score_compare (self->priv->style, oldscore, score) < 0) {
        self->scores_list = g_list_insert_before (self->scores_list,
						  s,
						  games_score_dup (score));
      }

      /* FIXME: write out the file. */

      s = g_list_next (s);
    }

    /* FIXME: Return the position it was inserted. 0 => Not inserted. */

    return 0;
 }
