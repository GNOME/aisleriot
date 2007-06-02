/*
 * games-scores.c
 *
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

#include "games-scores-backend.h"
#include "games-score.h"
#include "games-scores.h"

/* The local version of the GamesScoresCategory. */
typedef struct {
  gchar *key;
  gchar *name;
  GamesScoresBackend *backend;
} GamesScoresCategoryPrivate;

typedef struct _GamesScoresPrivate {
  GHashTable *categories;
  GSList *catsordered;
  gchar *currentcat;
  gchar *defcat;
  gchar *basename;
  gboolean last_score_significant;
  gint last_score_position;
  GamesScoreValue last_score_value;
  GamesScoreStyle style;
  GamesScoresCategoryPrivate dummycat;
} GamesScoresPrivate;

static void
games_scores_category_free (GamesScoresCategoryPrivate * cat)
{
  g_free (cat->key);
  g_free (cat->name);
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
static GamesScoresCategoryPrivate *
games_scores_get_current (GamesScores * self)
{
  GamesScoresCategoryPrivate *cat;

  if (self->priv->currentcat == NULL) {
    /* We have a single, anonymous, category. */
    cat = &(self->priv->dummycat);
  } else {
    cat =
      g_hash_table_lookup (self->priv->categories, self->priv->currentcat);
  }

  if (cat->backend == NULL) {
    cat->backend =
      games_scores_backend_new (self->priv->style, self->priv->basename,
				cat->key);
  }

  return cat;
}

/* FIXME: Static games_score_init function to initialise the setgid stuff. */
/* FIXME: This is actually an argument for a helper-app since this function
 * won't know what files we are after until _new is called. */

G_DEFINE_TYPE (GamesScores, games_scores, G_TYPE_OBJECT);

/** 
 * new:
 * @description: A GamesScoresDescription structure with the information
 *               about this games scoring system. 
 *
 * Create an object to handle a set of scores. Normally you will make one
 * global object. Creating and destroying these objects is inefficient. 
 * Using multipl objects referring to the same set of scores at the same
 * time should work but is unnecessary liable to be buggy.
 */
GamesScores *
games_scores_new (const GamesScoresDescription * description)
{
  GamesScores *self;
  const GamesScoresCategory *cats;

  self = GAMES_SCORES (g_object_new (GAMES_TYPE_SCORES, NULL));

  /* FIXME: Input sanity checks. */

  self->priv->categories =
    g_hash_table_new_full (g_str_hash, g_str_equal,
			   g_free,
			   (GDestroyNotify) games_scores_category_free);

  /* catsordered is a record of the ordering of the categories. 
   * Its data is shared with the hash table. */
  self->priv->catsordered = NULL;

  if (description->categories) {
    cats = description->categories;
    while (cats->key) {
      games_scores_add_category (self, cats->key, cats->name);
      cats++;
    }

    self->priv->defcat = g_strdup (description->deflt);
    self->priv->currentcat = g_strdup (self->priv->defcat);
  } else {
    self->priv->currentcat = NULL;
    self->priv->defcat = NULL;
    self->priv->catsordered = NULL;
  }

  self->priv->basename = g_strdup (description->basename);
  /* FIXME: Do some sanity checks on the default and the like. */

  self->priv->style = description->style;

  /* Set up the anonymous category for use when no categories are specified. */
  self->priv->dummycat.key = "";
  self->priv->dummycat.name = "";
  self->priv->dummycat.backend = NULL;

  return self;
}

/**
 * add_category:
 * @scores: A scores object.
 * @key: The key for the new category.
 * @name: The player-visible label for the new category.
 *
 * Add a new category after initialisation. key and name are copied into
 * internal structures. The scores dialog is not currently updated.
 *
 **/
void
games_scores_add_category (GamesScores * self, gchar * key, gchar * name)
{
  GamesScoresCategoryPrivate *cat;

  cat = g_new (GamesScoresCategoryPrivate, 1);
  cat->key = g_strdup (key);
  cat->name = g_strdup (name);
  cat->backend = NULL;

  g_hash_table_insert (self->priv->categories, g_strdup (key), cat);
  self->priv->catsordered = g_slist_append (self->priv->catsordered, cat);
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
  g_return_if_fail (self != NULL);
  g_return_if_fail (self->priv != NULL);

  if (category == NULL)
    category = self->priv->defcat;

  if (self->priv->currentcat)
    g_free (self->priv->currentcat);

  self->priv->currentcat = g_strdup (category);

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
  GamesScore *fullscore;
  GamesScoresCategoryPrivate *cat;
  gint place, n;
  GList *s, *scores_list;

  g_return_val_if_fail (self != NULL, 0);
  g_return_val_if_fail (self->priv != NULL, 0);

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
    if (games_score_compare (self->priv->style, oldscore, fullscore) < 0) {
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

  self->priv->last_score_significant = place > 0;
  self->priv->last_score_position = place;
  self->priv->last_score_value = score;

  return place;
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
  GamesScoresCategoryPrivate *cat;
  GList *s, *scores_list;
  gint n, place;
  gchar *old_name;
  GamesScore *sc;
  GamesScoreValue score;

  g_return_if_fail (self != NULL);
  g_return_if_fail (self->priv != NULL);

  place = self->priv->last_score_position;
  score = self->priv->last_score_value;

  if (place == 0)
    return;

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
    if ((games_score_compare_values (self->priv->style, sc->value, score) ==
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
  GamesScoresCategoryPrivate *cat;
  GList *scores;

  g_return_val_if_fail (self != NULL, NULL);
  g_return_val_if_fail (self->priv != NULL, NULL);

  cat = games_scores_get_current (self);

  scores = games_scores_backend_get_scores (cat->backend);
  /* Tell the backend that we won't be altering the scores so it
   * can release the lock. */
  games_scores_backend_discard_scores (cat->backend);

  return scores;
}

/**
 * category_foreach:
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
games_scores_category_foreach (GamesScores * self,
			       GamesScoresCategoryForeachFunc func,
			       gpointer userdata)
{
  GSList *list;
  GamesScoresCategory temp;

  g_return_if_fail (self != NULL);
  g_return_if_fail (self->priv != NULL);

  list = self->priv->catsordered;
  while (list) {
    temp.key = ((GamesScoresCategoryPrivate *) list->data)->key;
    temp.name = ((GamesScoresCategoryPrivate *) list->data)->name;

    (*func) (&temp, userdata);
    list = g_slist_next (list);
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
  g_return_val_if_fail (self != NULL, 0);
  g_return_val_if_fail (self->priv != NULL, 0);

  return self->priv->style;
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
  g_return_val_if_fail (self != NULL, NULL);
  g_return_val_if_fail (self->priv != NULL, NULL);

  return self->priv->currentcat;
}

static void
games_scores_init (GamesScores * self)
{
  /* Most of the work is done in the _new method. */

  self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self, GAMES_TYPE_SCORES,
					    GamesScoresPrivate);

  self->priv->last_score_significant = FALSE;
  self->priv->last_score_position = 0;
  self->priv->last_score_value.plain = 0;
}

static void
games_scores_class_init (GamesScoresClass * klass)
{
  g_type_class_add_private (klass, sizeof (GamesScoresPrivate));
}
