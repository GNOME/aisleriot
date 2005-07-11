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

#include <gtk/gtk.h>


#include <config.h>

#include <gnome.h>

#include <math.h>
#include <fcntl.h>
#include <unistd.h>

#include "games-scores-dialog.h"
#include "games-scores-backend.h"
#include "games-score.h"
#include "games-scores.h"

/* We want GamesScoresCategory to be a plain structure so it can easily
 * be initialised at compile time (to make writing a GamesScoresDescription
 * easy). However we do need some common methods. These functions
 * are here to give us our "pseudo-object". */

static void games_scores_category_free (GamesScoresCategory *cat) {
  g_free (cat->key);
  g_free (cat->name);
  g_free (cat);
}

static GamesScoresCategory *games_scores_category_dup (GamesScoresCategory *orig) {
  GamesScoresCategory *newcat;
  
  newcat = g_new (GamesScoresCategory, 1);
  newcat->key = g_strdup (orig->key);
  newcat->name = g_strdup (orig->name);
  
  return newcat;
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
GObject * games_scores_new (GamesScoresDescription * description) {
  GamesScores *self;
  GamesScoresCategory *cats;
  GamesScoresCategory *dupcat;
  
  self = GAMES_SCORES (g_object_new (GAMES_TYPE_SCORES, NULL));

  /* FIXME: Input sanity checks. */
  
  cats = description->categories;
  self->priv->categories = 
    g_hash_table_new_full (g_str_hash, g_str_equal,
			   g_free, 
			     (GDestroyNotify) games_scores_category_free);
  while (cats) {
    dupcat = games_scores_category_dup (cats);
    
    g_hash_table_insert (self->priv->categories, 
			 g_strdup (cats->key),
			 dupcat);
    cats++;
  }
  
  self->priv->defcat = g_strdup (description->deflt);
  self->priv->currentcat = g_strdup (self->priv->defcat);
  /* FIXME: Do some sanity checks on the default and the like. */
  
  self->priv->style = description->style;

  self->priv->backend = games_scores_backend_new (description->style,
						  description->filename);
      
  return (GObject *) self;
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
 * scores are to be stored and what the default score display is when the
 * dialog is requested.
 *
 **/
void games_scores_set_category (GamesScores *self, gchar *category) {

  g_return_if_fail (self != NULL);
  
  if (category == NULL)
    category = self->priv->defcat;
  
  if (self->priv->currentcat)
    g_free (self->priv->currentcat);
  
  self->priv->currentcat = g_strdup (category);
  
  if (self->priv->dialog != NULL)
    games_scores_dialog_set_category (GAMES_SCORES_DIALOG (self->priv->dialog),
				      category);
  
  /* FIXME: Check validity of category (Null, the same as current, 
   * is actually a category) then just set it in the structure. */
}

/**
 * add_score:
 * @scores: A scores object.
 * @score: A GamesScore, please use the macros to convert a score to the
 *         gpointer format.
 *
 * Add a score to the set of scores. Retention of anything but the
 * top-ten scores is undefined. It returns a boolean indicating whether
 * the score is a top-ten one or not.
 **/
/* FIXME: Write the above-mentioned macros. */
gboolean games_scores_add_score (GamesScores *self, GamesScore score) {

  /* Start by assuming that it isn't a top-ten score. */
  self->priv->last_score_significant = FALSE;
  
  /* FIXME: Fill in. */
  /* Lock the scores file and get the list. */
  
  /* Check if our score belongs in the list. */
  
  /* If so insert it and rewrite the list. */
  
  /* Free the lock. */
  
  return self->priv->last_score_significant;
}

static void 
games_scores_set_dialog_categories (gchar *key, 
				    GamesScoresCategory *cat,
				    GamesScoresDialog *dialog)
{
  games_scores_dialog_add_category (dialog, key, cat->name);
}

/**
 * show:
 * @scores: A scores object.
 * @hilight: Whether or not to hilight the last high score set (if
 *           it was a top-ten score).
 *
 * The basic function for showing a high scores dialog. All the details
 * are taken care of for you, you merely have to decide on whether this
 * is a user interested in the historic high scores or if it is a report 
 * on the last game played.
 */
void games_scores_show (GamesScores *self, gboolean hilight) {
  GtkWidget *dialog;
    
  if (self->priv->dialog == NULL) {
    /* FIXME: The title is a problem. */
    dialog = games_scores_dialog_new (NULL, "This title is wrong");
    
    g_hash_table_foreach (self->priv->categories, 
			  (GHFunc) games_scores_set_dialog_categories,
			  dialog);
    
    /* FIXME: Also fill in the buttons and the message and the like. 
     * This may have to be done below. */
    
    games_scores_dialog_set_category (GAMES_SCORES_DIALOG (dialog),
				      self->priv->currentcat);
    
    self->priv->dialog = dialog;
  } else {
    /* FIXME: Probably should move this beyond the code that
     * sets the hilight and the like. */
    gtk_window_present (GTK_WINDOW (self->priv->dialog));
  }
  
  if (hilight) {
    games_scores_dialog_set_hilight (GAMES_SCORES_DIALOG (self->priv->dialog),
				     self->priv->last_score_position);
  }
  
}

static void
games_scores_init (GamesScores *self) {
  /* Most of the work is done in the _new method. */

  self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self, GAMES_TYPE_SCORES,
					    GamesScoresPrivate);
  
  self->priv->last_score_significant = FALSE;
  self->priv->last_score_position = -1; /* FIXME: = 0? */
  
  self->priv->dialog = NULL;
}

static void
games_scores_class_init (GamesScoresClass *klass) {
  g_type_class_add_private (klass, sizeof (GamesScoresPrivate));
}
