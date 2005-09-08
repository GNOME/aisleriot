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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "games-score.h"
#include "games-scores.h"
#include "games-scores-backend.h"

G_DEFINE_TYPE (GamesScoresBackend, games_scores_backend, G_TYPE_OBJECT);

static void
games_scores_backend_finalize (GamesScoresBackend *backend)
{

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
					      gchar *basename,
					      gchar *name)
{
  GamesScoresBackend *backend;
  gchar *fullname;

  backend = GAMES_SCORES_BACKEND (g_object_new (GAMES_TYPE_SCORES_BACKEND, 
						NULL));

  if (name[0] == '\0') /* Name is "" */
    fullname = g_strjoin (".", basename, "scores", NULL);
  else
    fullname = g_strjoin (".", basename, name, "scores", NULL);

  backend->priv->style = style;
  backend->scores_list = NULL;
  backend->priv->filename = g_build_filename (SCORESDIR, fullname, NULL);
  g_free (fullname);

  return backend;
}

/* You can alter the list returned by this function, but you must
 * make sure you set it again with the _set_scores method
 * before calling _get_scores again. */
GList *games_scores_backend_get_scores (GamesScoresBackend *self) 
{
  gchar *buffer;
  gchar *eol;
  gchar *scorestr;
  gchar *timestr;
  gchar *namestr;
  gsize blen;
  gboolean errorp;
  GamesScore *newscore;

  /* Check for a change in the scores file and update if necessary. */

  /* FIXME: Update the list when necessary, don't just cache it forever. */
  if ((self->scores_list == NULL) /* || something */ ) {
    /* Lock the file and get the list. */
    /* FIXME: Lock the file. */
    errorp = g_file_get_contents (self->priv->filename, 
				  &buffer, &blen, NULL);
    
    if (!errorp || (blen == 0)) {
      return NULL;
    }

    /* FIXME: These details should be in a sub-class. */

    /* Parse the list. We start by breaking it into lines. */
    /* Since the buffer is null-terminated by g_file_get_contents, 
     * we can do the string stuff reasonably safely. */
    eol = strchr (buffer, '\n');
    scorestr = buffer;
    /* FIXME: Locale issues for parsing the numbers? i.e. should
    * we set it back to C. */
    while (eol != NULL) {
      *eol++ = '\0';
      timestr = strchr (scorestr, ' ');
      if (timestr == NULL) break;
      *timestr++ = '\0';
      namestr = strchr (timestr, ' ');
      if (namestr == NULL) break;
      *namestr++ = '\0';
      /* At this point we have three strings, all null terminated. All
       * part of the original buffer. */
      newscore = games_score_new ();
      newscore->name = g_strdup (namestr);
      newscore->time = strtoll (timestr, NULL, 10);
      switch (self->priv->style) {
      case GAMES_SCORES_STYLE_PLAIN_DESCENDING:
      case GAMES_SCORES_STYLE_PLAIN_ASCENDING:
	newscore->value.plain = strtod (scorestr, NULL);
	break;
      case GAMES_SCORES_STYLE_TIME_DESCENDING:
      case GAMES_SCORES_STYLE_TIME_ASCENDING:
	newscore->value.time_double = strtod (scorestr, NULL);
	break;
      }
      self->scores_list = g_list_append (self->scores_list, newscore);
      /* Setup again for the next time around. */
      scorestr = eol;
      eol = strchr (eol, '\n');
    }

    g_free (buffer);
  } 

  /* FIXME: Sort the scores! We shouldn't rely on the file being sorted. */

  return self->scores_list;
}

void games_scores_backend_set_scores (GamesScoresBackend *self,
				      GList *list)
{
    GList *s;
    GamesScore *d;
    FILE *f;

    /* FIXME: Lock the file. */

    /* FIXME: File mode. */

    /* Yeah! Good old-fashioned stdio. It suits us here. */
    f = fopen (self->priv->filename, "w");

    if (f == NULL) {
      g_warning ("Failed to open the high scores file %s: %s\n", 
		 self->priv->filename, strerror (errno));
      return;
    }

    self->scores_list = list;

    s = list;
    while (s != NULL) {
      gdouble rscore;
      guint64 rtime;
      gchar *rname;

      d = (GamesScore *)s->data;
      rscore = 0.0;
      switch (self->priv->style) {
      case GAMES_SCORES_STYLE_PLAIN_DESCENDING:
      case GAMES_SCORES_STYLE_PLAIN_ASCENDING:
	rscore = d->value.plain;
	break;
      case GAMES_SCORES_STYLE_TIME_DESCENDING:
      case GAMES_SCORES_STYLE_TIME_ASCENDING:
	rscore = d->value.time_double;
	break;
      }
      rtime = d->time;
      rname = d->name;

      /* We ignore the error and just keep trying. It will give 
       * better results than if we just gave up. */
      fprintf (f, "%g %lld %s\n", rscore, rtime, rname); 

      s = g_list_next (s);
    }

    fclose (f);

    /* FIXME: Unlock the file. */

 }
