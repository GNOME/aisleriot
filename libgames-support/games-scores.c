/*
 * Copyright 2005 Callum McKenzie
 *
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

/* FIXME: Document */

/* FIXME: Add a finaliser to get rid of some of the strings. */

#include <config.h>

#include <math.h>
#include <fcntl.h>
#include <unistd.h>

#include <glib/gi18n.h>

#include "games-scores-backend.h"
#include "games-score.h"
#include "games-scores.h"

/* The local version of the GamesScoresCategory. */
typedef struct {
  GamesScoresCategory category;
  GamesScoresBackend *backend;
} GamesScoresCategoryInternal;

struct _GamesScoresPrivate {
  GHashTable *categories;
  GSList *catsordered;
  gchar *currentcat;
  gchar *defcat;
  gchar *basename;
  gboolean last_score_significant;
  gint last_score_position;
  GamesScoreValue last_score_value;
  GamesScoreStyle style;
  GamesScoresCategoryInternal dummycat;
};

static void
games_scores_category_free (GamesScoresCategoryInternal *cat)
{
  g_free (cat->category.key);
  g_free (cat->category.name);
  if (cat->backend)
    g_object_unref (cat->backend);
  g_free (cat);
}

/**
 * get_current:
 * @self: A scores object.
 *
 * Retrieves the current category and make sure it is in a state to be used.
 *
 **/
static GamesScoresCategoryInternal *
games_scores_get_current (GamesScores * self)
{
  GamesScoresPrivate *priv = self->priv;
  GamesScoresCategoryInternal *cat;

  if (priv->currentcat == NULL) {
    /* We have a single, anonymous, category. */
    cat = &(priv->dummycat);
  } else {
    cat = g_hash_table_lookup (priv->categories, priv->currentcat);
    if (!cat)
      return NULL;
  }

  if (cat->backend == NULL) {
    cat->backend = games_scores_backend_new (priv->style, priv->basename,
                                             cat->category.key);
  }

  return cat;
}

/* FIXME: Static games_score_init function to initialise the setgid stuff. */
/* FIXME: This is actually an argument for a helper-app since this function
 * won't know what files we are after until _new is called. */

G_DEFINE_TYPE (GamesScores, games_scores, G_TYPE_OBJECT);

/** 
 * games_scores_new:
 * @app_name: the (old) app name (for backward compatibility),
 *   used as the basename of the category filenames
 * @categories: the score categories, or %NULL to use an anonymous category
 * @n_categories: the number of category entries in @categories
 * @categories_context: the translation context to use for the category names,
 *   or %NULL to use no translation context
 * @categories_domain: the translation domain to use for the category names,
 *   or %NULL to use the default domain
 * @default_category: the key of the default category, or %NULL
 * @style: the category style
 * 
 *
 * Returns: a new #GamesScores object
 */
GamesScores *
games_scores_new (const char *app_name,
                  const GamesScoresCategory *categories,
                  int n_categories,
                  const char *categories_context,
                  const char *categories_domain,
                  int default_category_index,
                  GamesScoreStyle style)
{
  GamesScores *self;
  GamesScoresPrivate *priv;

  self = GAMES_SCORES (g_object_new (GAMES_TYPE_SCORES, NULL));
  priv = self->priv;

  /* FIXME: Input sanity checks. */

  priv->categories = g_hash_table_new_full (g_str_hash, g_str_equal,
                                            g_free,
                                            (GDestroyNotify) games_scores_category_free);

  /* catsordered is a record of the ordering of the categories. 
   * Its data is shared with the hash table. */
  priv->catsordered = NULL;

  if (n_categories > 0) {
    int i;

    g_return_val_if_fail (default_category_index >= 0 && default_category_index < n_categories, NULL);

    for (i = 0; i < n_categories; ++i) {
      const GamesScoresCategory *category = &categories[i];
      const char *display_name;

      if (categories_context) {
        display_name = g_dpgettext2 (categories_domain, categories_context, category->name);
      } else {
        display_name = dgettext (categories_domain, category->name);
      }

      games_scores_add_category (self, category->key, display_name);
    }

    priv->defcat = g_strdup (categories[default_category_index].key);
    priv->currentcat = g_strdup (priv->defcat);
  } else {
    priv->currentcat = NULL;
    priv->defcat = NULL;
  }

  priv->basename = g_strdup (app_name);
  /* FIXME: Do some sanity checks on the default and the like. */

  priv->style = style;

  /* Set up the anonymous category for use when no categories are specified. */
  priv->dummycat.category.key = "";
  priv->dummycat.category.name = "";
  priv->dummycat.backend = NULL;

  return self;
}

/**
 * games_scores_add_category:
 * @self:
 * @key: the key for the new category
 * @name: the user visible label for the new category
 *
 * Add a new category after initialisation. key and name are copied into
 * internal structures. The scores dialog is not currently updated.
 *
 **/
void
games_scores_add_category (GamesScores *self,
                           const char *key,
                           const char *name)
{
  GamesScoresPrivate *priv = self->priv;
  GamesScoresCategoryInternal *cat;

  cat = g_new (GamesScoresCategoryInternal, 1);
  cat->category.key = g_strdup (key);
  cat->category.name = g_strdup (name);
  cat->backend = NULL;

  g_hash_table_insert (priv->categories, g_strdup (key), cat);
  priv->catsordered = g_slist_append (priv->catsordered, cat);
}

/**
 * set_category:
 * @scores: A scores object.
 * @category: A string identifying the category to use (the key in
 *            the GamesScoresCategory structure).
 *
 * This function sets the scores category to use. e.g. whether we are playing
 * on hard, medium or easy. It should be used at the time that the game
 * itself switches between difficulty levels. The category determines where
 * scores are to be stored and read from.
 *
 **/
void
games_scores_set_category (GamesScores * self, gchar * category)
{
  GamesScoresPrivate *priv = self->priv;

  g_return_if_fail (self != NULL);

  if (category == NULL)
    category = priv->defcat;

  g_free (priv->currentcat);
  priv->currentcat = g_strdup (category);

  /* FIXME: Check validity of category (Null, the same as current, 
   * is actually a category) then just set it in the structure. */
}

/**
 * add_score:
 * @self: A scores object.
 * @score: A GamesScoreValue - it is up to the caller to convert their
 *         raw value to one of the supported types.
 *
 * Add a score to the set of scores. Retention of anything but the
 * top-ten scores is undefined. It returns either the place in the top ten
 * or zero if no place was achieved. It can therefore be treated as a
 * boolean if desired.
 *
 **/
gint
games_scores_add_score (GamesScores * self, GamesScoreValue score)
{
  GamesScoresPrivate *priv = self->priv;
  GamesScore *fullscore;
  GamesScoresCategoryInternal *cat;
  gint place, n;
  GList *s, *scores_list;

  g_return_val_if_fail (self != NULL, 0);

  fullscore = games_score_new ();
  fullscore->value = score;

  cat = games_scores_get_current (self);

  scores_list = games_scores_backend_get_scores (cat->backend);

  s = scores_list;
  place = 0;
  n = 0;

  while (s != NULL) {
    GamesScore *oldscore = s->data;

    n++;

    /* If beat someone in the list, add us there. */
    if (games_score_compare (priv->style, oldscore, fullscore) < 0) {
      scores_list = g_list_insert_before (scores_list, s,
					  games_score_dup (fullscore));
      place = n;
      break;
    }

    s = g_list_next (s);
  }

  /* If we haven't placed anywhere and the list still has 
   * room to grow, put us on the end. 
   * This also handles the empty-file case. */
  if ((place == 0) && (n < GAMES_SCORES_SIGNIFICANT)) {
    place = n + 1;
    scores_list = g_list_append (scores_list, games_score_dup (fullscore));
  }

  if (g_list_length (scores_list) > GAMES_SCORES_SIGNIFICANT) {
    s = g_list_nth (scores_list, GAMES_SCORES_SIGNIFICANT - 1);
    /* Note that we are guaranteed to only need to remove one link
     * and it is also guaranteed not to be the first one. */
    games_score_destroy ((GamesScore *) (g_list_next (s)->data));
    g_list_free (g_list_next (s));
    s->next = NULL;
  }

  if (games_scores_backend_set_scores (cat->backend, scores_list) == FALSE)
    place = 0;

  priv->last_score_significant = place > 0;
  priv->last_score_position = place;
  priv->last_score_value = score;

  return place;
}

/**
 * games_scores_update_score_name:
 * @self: A scores object.
 * @new_name: The new name to use.
 *
 * By default add_score uses the current user name. This routine updates
 * that name. There are a few wrinkles: the score may have moved since we
 * got the original score. Use in normal code is discouraged, it is here 
 * to be used by GamesScoresDialog.
 *
 **/
void
games_scores_update_score_name (GamesScores * self, gchar * new_name, gchar * old_name)
{
  GamesScoresPrivate *priv = self->priv;
  GamesScoresCategoryInternal *cat;
  GList *s, *scores_list;
  gint n, place;
  GamesScore *sc;
  GamesScoreValue score;

  g_return_if_fail (self != NULL);

  place = priv->last_score_position;
  score = priv->last_score_value;

  if (place == 0)
    return;

  if (old_name)
      old_name = g_strdup (old_name); /* Make copy so we can free it later */
  else
      old_name = g_strdup (g_get_real_name ());

  cat = games_scores_get_current (self);

  scores_list = games_scores_backend_get_scores (cat->backend);

  s = g_list_last (scores_list);
  n = g_list_length (scores_list);

  /* We hunt backwards down the list until we find the last entry with
   * a matching user and score. */
  /* The check that we haven't gone back before place isn't just a
   * pointless optimisation. It also catches the case where our score
   * has been dropped from the high-score list in the meantime. */

  while ((n >= place) && (s != NULL)) {
    sc = (GamesScore *) (s->data);
    if ((games_score_compare_values (priv->style, sc->value, score) ==
	 0) && (g_utf8_collate (old_name, sc->name) == 0)) {
      g_free (sc->name);
      sc->name = g_strdup (new_name);
    }

    s = g_list_previous (s);
    n--;
  }

  games_scores_backend_set_scores (cat->backend, scores_list);

  g_free (old_name);
}

/**
 * games_scores_update_score:
 * @self: A scores object.
 * @new_name: The new name to use.
 *
 * By default add_score uses the current user name. This routine updates
 * that name. There are a few wrinkles: the score may have moved since we
 * got the original score. Use in normal code is discouraged, it is here 
 * to be used by GamesScoresDialog.
 *
 **/
void
games_scores_update_score (GamesScores * self, gchar * new_name)
{
    games_scores_update_score_name (self, new_name, NULL);
}

/**
 * get:
 * @self: A scores object.
 *
 * Get a list of GamesScore objects for the current category. The list
 * is still owned by the GamesScores object and is not guaranteed to
 * be the either the same or accurate after any games_scores call
 * except games_scores_get. Do not alter the data either.
 **/
GList *
games_scores_get (GamesScores * self)
{
  GamesScoresCategoryInternal *cat;
  GList *scores;

  g_return_val_if_fail (self != NULL, NULL);

  cat = games_scores_get_current (self);

  scores = games_scores_backend_get_scores (cat->backend);
  /* Tell the backend that we won't be altering the scores so it
   * can release the lock. */
  games_scores_backend_discard_scores (cat->backend);

  return scores;
}

/**
 * _games_scores_category_foreach:
 * @self: A scores object.
 * @func: A function to call.
 * @userdata: Arbitrary data.
 *
 * This function will iterate over the list of categories calling the
 * supplied function with the category and userdata as arguments.
 * The ordering of the categories is the order they were added.
 *
 **/
void
_games_scores_category_foreach (GamesScores * self,
                                GamesScoresCategoryForeachFunc func,
                                gpointer userdata)
{
  GamesScoresPrivate *priv = self->priv;
  GSList *l;

  g_return_if_fail (self != NULL);

  for (l = priv->catsordered; l != NULL; l = l->next) {
    func ((GamesScoresCategory*) l->data, userdata);
  }
}

/**
 * get_style:
 * @self: A scores object.
 *
 * Returns the style of the scores.
 *
 **/
GamesScoreStyle
games_scores_get_style (GamesScores * self)
{
  GamesScoresPrivate *priv = self->priv;

  g_return_val_if_fail (self != NULL, 0);

  return priv->style;
}

/**
 * get_category:
 * @self: A scores object.
 *
 * Returns the current category key. It is owned by the GamesScores object and
 * should not be altered. This will be NULL if no category is current (this
 * will typically happen if no categories have been added to the GamesScore).
 *
 **/
const gchar *
games_scores_get_category (GamesScores * self)
{
  GamesScoresPrivate *priv = self->priv;

  g_return_val_if_fail (self != NULL, NULL);

  return priv->currentcat;
}

static void
games_scores_init (GamesScores * self)
{
  GamesScoresPrivate *priv;

  /* Most of the work is done in the _new method. */

  priv = self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self, GAMES_TYPE_SCORES, GamesScoresPrivate);

  priv->last_score_significant = FALSE;
  priv->last_score_position = 0;
  priv->last_score_value.plain = 0;
}

static void
games_scores_class_init (GamesScoresClass * klass)
{
  g_type_class_add_private (klass, sizeof (GamesScoresPrivate));
}
