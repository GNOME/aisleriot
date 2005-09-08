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

#include <gnome.h>

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

/* Note that these pseudo-methods don't quite do what they say they
   do. In particular category_dup doesn't return what it is
   given. This is all because there are different public and private
   representations of the data. */

static void games_scores_category_free (GamesScoresCategoryPrivate *cat) {
  g_free (cat->key);
  g_free (cat->name);
  if (cat->backend)
    g_object_unref (cat->backend);
  g_free (cat);
}

static GamesScoresCategoryPrivate *games_scores_category_dup (const GamesScoresCategory *orig) {
  GamesScoresCategoryPrivate *newcat;
  
  newcat = g_new0 (GamesScoresCategoryPrivate, 1);
  newcat->key = g_strdup (orig->key);
  newcat->name = g_strdup (orig->name);
  newcat->backend = NULL; /* Backends are created on demand. */

  return newcat;
}

/**
 * get_current:
 * @self: A scores object.
 *
 * Retrieves the current category and make sure it is in a state to be used.
 *
 **/
static GamesScoresCategoryPrivate *games_scores_get_current (GamesScores *self)
{
  GamesScoresCategoryPrivate *cat;

  cat = g_hash_table_lookup (self->priv->categories, self->priv->currentcat);

  if (cat->backend == NULL) { 
   cat->backend = games_scores_backend_new (self->priv->style, self->priv->basename, cat->key); 
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
GamesScores * games_scores_new (const GamesScoresDescription * description) {
  GamesScores *self;
  const GamesScoresCategory *cats;
  GamesScoresCategoryPrivate *dupcat;
  
  self = GAMES_SCORES (g_object_new (GAMES_TYPE_SCORES, NULL));

  /* FIXME: Input sanity checks. */
  
  cats = description->categories;
  self->priv->categories = 
    g_hash_table_new_full (g_str_hash, g_str_equal,
			   g_free, 
			     (GDestroyNotify) games_scores_category_free);
  while (cats->key) {
    dupcat = games_scores_category_dup (cats);
    
    g_hash_table_insert (self->priv->categories, 
			 g_strdup (cats->key),
			 dupcat);
    cats++;
  }
  
  self->priv->defcat = g_strdup (description->deflt);
  self->priv->currentcat = g_strdup (self->priv->defcat);
  self->priv->basename = g_strdup (description->basename);
  /* FIXME: Do some sanity checks on the default and the like. */
  
  self->priv->style = description->style;
      
  return self;
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
void games_scores_set_category (GamesScores *self, gchar *category) {

  g_return_if_fail (self != NULL);

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
 * @score: A GamesScore, please use the macros to convert a score to the
 *         gpointer format.
 *
 * Add a score to the set of scores. Retention of anything but the
 * top-ten scores is undefined. It returns either the place in the top ten
 * or zero if no place was achieved. It can therefore be treated as a
 * boolean if desired.
 *
 **/
/* FIXME: Write the above-mentioned macros. */
gint games_scores_add_score (GamesScores *self, GamesScoreValue score) {
  GamesScore *fullscore;
  GamesScoresCategoryPrivate *cat;
  gint place, n;
  GList *s, *scores_list;

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
    games_score_destroy ((GamesScore *)(g_list_next (s)->data));
    g_list_free (g_list_next (s));
    s->next = NULL;
  }
  
  games_scores_backend_set_scores (cat->backend, scores_list);
  
  self->priv->last_score_significant = place > 0;
  
  return place;
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
GList *games_scores_get (GamesScores *self)
{
  GamesScoresCategoryPrivate *cat;

  cat = games_scores_get_current (self);

  return games_scores_backend_get_scores (cat->backend);
}

typedef struct {
  GamesScoresCategoryForeachFunc func;
  gpointer userdata;
} GamesScoresCategoryForeachHelper;

static void games_scores_category_foreach_helper (gchar *key, 
						  GamesScoresCategoryPrivate *cat, 
						  GamesScoresCategoryForeachHelper *h)
{
  GamesScoresCategory temp;

  temp.key = cat->key;
  temp.name = cat->name;

  (*h->func)(&temp, h->userdata);
}
					   

/**
 * category_foreach:
 * @self: A scores object.
 * @func: A function to call.
 * @userdata: Arbitrary data.
 *
 * This function will iterate over the list of categories calling the
 * supplied function with the category and userdata as arguments.
 *
 **/
void games_scores_category_foreach (GamesScores *self, 
				    GamesScoresCategoryForeachFunc func, 
				    gpointer userdata)
{
  GamesScoresCategoryForeachHelper params;

  params.func = func;
  params.userdata = userdata;

  g_hash_table_foreach (self->priv->categories, 
			(GHFunc) games_scores_category_foreach_helper, &params);
}

/**
 * get_style:
 * @self: A scores object.
 *
 * Returns the style of the scores.
 *
 **/
GamesScoreStyle games_scores_get_style (GamesScores *self)
{
  return self->priv->style;
}

/**
 * get_category:
 * @self: A scores object.
 *
 * Returns the current category key. It is owned by the GamesScores object and
 * should not be altered.
 *
 **/
const gchar *games_scores_get_category (GamesScores *self)
{
  return self->priv->currentcat;
}

static void
games_scores_init (GamesScores *self) {
  /* Most of the work is done in the _new method. */

  self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self, GAMES_TYPE_SCORES,
					    GamesScoresPrivate);
  
  self->priv->last_score_significant = FALSE;
  self->priv->last_score_position = -1; /* FIXME: = 0? */
}

static void
games_scores_class_init (GamesScoresClass *klass) {
  g_type_class_add_private (klass, sizeof (GamesScoresPrivate));
}

