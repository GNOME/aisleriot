/* -*- mode: C -*-

   games-scores-dialog.c
   Copyright 2004, 2005, 2006 Callum McKenzie

   This library is free software; you can redistribute it and'or modify
   it under the terms of the GNU Library General Public License as published
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#include <config.h>

#include <math.h>

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "games-scores-dialog.h"
#include "games-scores-dialog-private.h"

G_DEFINE_TYPE (GamesScoresDialog, games_scores_dialog, GTK_TYPE_DIALOG);

static void
games_scores_dialog_finalize (GObject *o)
{
  GamesScoresDialog *dialog = GAMES_SCORES_DIALOG (o);

  if (dialog->_priv->scores)
    g_object_unref (dialog->_priv->scores);

  if (dialog->_priv->categories)
    g_hash_table_destroy (dialog->_priv->categories);
  if (dialog->_priv->catindices)
    g_hash_table_destroy (dialog->_priv->catindices);

  G_OBJECT_CLASS (games_scores_dialog_parent_class)->finalize (o);
}

static void
games_scores_dialog_class_init (GamesScoresDialogClass *klass)
{
  g_type_class_add_private (klass, sizeof (GamesScoresDialogPrivate));

  G_OBJECT_CLASS (klass)->finalize = games_scores_dialog_finalize;
}

/**
 * add_category:
 * @self: a pointer to a GamesScoresDialog
 * @key: an identifier for the category. This should also be a valid
 * score category for the gnome_score system.
 * @name: the category name
 * 
 * Adds a new category to combo box selector.
 *
 **/
static void games_scores_dialog_add_category (GamesScoresDialog *self, 
				       const gchar *key, 
				       const gchar *name)
{
  gchar *k;

  k = g_strdup (key);

  g_hash_table_insert (self->_priv->categories, k, 
			 GINT_TO_POINTER (self->_priv->catcounter));
  g_hash_table_insert (self->_priv->catindices, 
			 GINT_TO_POINTER (self->_priv->catcounter),
			 k);
  self->_priv->catcounter++;
  gtk_combo_box_append_text (GTK_COMBO_BOX (self->_priv->combo), name);
}

/* This is a helper function for loading the initial list of categories
 * in a _foreach function. If only C had lambda... */
static void games_scores_dialog_load_categories (GamesScoresCategory *cat, 
						 GamesScoresDialog *self) 
{
    /* category->name is already translated here! */
    games_scores_dialog_add_category (self, cat->key, cat->name);
}

/**
 * set_style:
 * @self: a pointer to a GamesScoresDialog
 * @style: the style to use
 * 
 * Sets the style of score displayed. e.g. GAMES_SCORES_STYLE_TIME
 * displays the scores as times. Note that the order of the scores
 * is determined at the gnome_score layer but their interpretation
 * is at this layer.
 *
 **/
static void games_scores_dialog_set_style (GamesScoresDialog *self, 
					   GamesScoreStyle style) 
{
  gchar *header;

  self->_priv->style = style;
  switch (style) {
  case GAMES_SCORES_STYLE_TIME_DESCENDING:
  case GAMES_SCORES_STYLE_TIME_ASCENDING:
    header = _("Time");
    break;
  case GAMES_SCORES_STYLE_PLAIN_DESCENDING:
  case GAMES_SCORES_STYLE_PLAIN_ASCENDING:
  default:
    header = _("Score");
  }

  gtk_tree_view_column_set_title (self->_priv->column, header);
}

/**
 * set_category:
 * @self: a pointer to a GamesScoresDialog
 * @key: the category to change to
 * 
 * Sets the category the scores dialog is displaying.
 *
 **/
static void games_scores_dialog_set_category (GamesScoresDialog *self, 
					      const gchar *key) 
{
  gpointer value;
  int idx;

  value = g_hash_table_lookup (self->_priv->categories, key);
  idx = GPOINTER_TO_INT (value);

  self->_priv->preservehilight = TRUE;
  gtk_combo_box_set_active (GTK_COMBO_BOX (self->_priv->combo), idx);
}

/**
 * new:
 * @domain: the scores domain to use, usually the application name
 * @title: the title for the dialog
 * 
 * Creates a new high scores dialog. Use gtk_dialog_run and 
 * gtk_widget_destroy to manage it.
 *
 * Returns: a new widget
 *
 **/
GtkWidget * games_scores_dialog_new (GtkWindow *parent_window, GamesScores *scores, const gchar *title)
{
  GamesScoresDialog *dialog = GAMES_SCORES_DIALOG (g_object_new (GAMES_TYPE_SCORES_DIALOG, NULL));

  dialog->_priv->scores = g_object_ref (scores);
  games_scores_dialog_set_style (dialog, games_scores_get_style (scores));
  dialog->_priv->preservehilight = FALSE;

  gtk_window_set_title (GTK_WINDOW (dialog), title);
  gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent_window));

  _games_scores_category_foreach (scores, 
                                  (GamesScoresCategoryForeachFunc) games_scores_dialog_load_categories,
                                  dialog);

  if (dialog->_priv->catcounter <= 1) {
    gtk_widget_hide (dialog->_priv->catbar);
  }

  return (GtkWidget *)dialog;
}

/* Retrieve the edited name from a new high score. */
static void games_scores_dialog_name_edited (GtkCellRendererText *cell, 
					     gchar *path, gchar *new_text, 
					     GamesScoresDialog *self)
{
  GtkTreeIter iter;
  gchar *old_name = NULL;

  gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (self->_priv->list), 
				       &iter, path);

  /* Get old name for comparison */
  gtk_tree_model_get (GTK_TREE_MODEL (self->_priv->list),
                      &iter, 0, &old_name, -1);
                           
  gtk_list_store_set (self->_priv->list, &iter, 0, new_text, -1);

  games_scores_update_score_name (self->_priv->scores, new_text, old_name);
}

/* Prevent editing of any cell in the high score list but the one we set. */
static void games_scores_dialog_cursor_changed (GtkTreeView *treeview, 
						GamesScoresDialog *self)
{
  g_object_set (self->_priv->namerenderer, "editable", FALSE, NULL);
}    

/* These contortions are to ensure that only the single most-recent
 * entry can be edited. */
static gboolean games_scores_dialog_set_edit (GamesScoresDialog *self)
{
  GtkTreePath *path;
  GtkTreeSelection *selection;

  /* Just in case we've been closed as soon as we're created. */
  if (!GTK_WIDGET_REALIZED (self))
    return FALSE;

  /* Temporarily disable the code that prevents editing when the
   * cursor changes position. */
  g_signal_handler_block (self->_priv->treeview, 
			    self->_priv->cursor_handler_id); 
  g_object_set (self->_priv->namerenderer, "editable", TRUE, NULL);
  selection = gtk_tree_view_get_selection (self->_priv->treeview);
  path = gtk_tree_path_new_from_indices (self->_priv->hilight - 1, -1);
  gtk_tree_selection_select_path (selection, path);
  gtk_tree_view_set_cursor (self->_priv->treeview, path, 
			      self->_priv->namecolumn, TRUE);
  g_signal_handler_unblock (self->_priv->treeview, 
			      self->_priv->cursor_handler_id); 
  gtk_tree_path_free (path);

  return FALSE;
}

/* Yet another part of the puzzle that lets the correct high-score be
 * editable. */
static void games_scores_dialog_set_hilight_private (GamesScoresDialog *self) 
{
  if (self->_priv->hilight == 0) {
    g_object_set (self->_priv->namerenderer, "editable", FALSE, NULL);
    return;
  }

  if (self->_priv->hilight == self->_priv->sethilight)
    return;

  self->_priv->sethilight = self->_priv->hilight;

  /* We can't set the hilight editable immediately in case we are
   * still in the process of being created and the editing subwindow
   * gets put in the wrong place. Attaching to the expose signal
   * doesn't seem to have the desired effect, so instead we just
   * wait until all other work is done. */
  g_idle_add ((GSourceFunc)games_scores_dialog_set_edit, self);
}

/* Load up the list with the current set of scores. */
static void games_scores_dialog_redraw (GamesScoresDialog *self) {
  GtkTreeIter iter;
  gchar *name;
  gint score;
  gchar *ss;
  gdouble dscore;
  GList *scorelist;

  gtk_list_store_clear (self->_priv->list);

  scorelist = games_scores_get (self->_priv->scores);

  while (scorelist) {
    name = ((GamesScore *)scorelist->data)->name;
    switch (self->_priv->style) {
    case GAMES_SCORES_STYLE_TIME_ASCENDING:
    case GAMES_SCORES_STYLE_TIME_DESCENDING:
      dscore = ((GamesScore *)scorelist->data)->value.time_double;
      score = rint (100*(dscore));
      /* Translators: this is for a minutes, seconds time display. */
      ss = g_strdup_printf (_("%dm %ds"), score/100, score%100);
      break; 
    case GAMES_SCORES_STYLE_PLAIN_ASCENDING:
    case GAMES_SCORES_STYLE_PLAIN_DESCENDING:
    default:
      score = ((GamesScore *)scorelist->data)->value.plain;
      ss = g_strdup_printf ("%d", score);
    }
    gtk_list_store_append (self->_priv->list, &iter);
    gtk_list_store_set (self->_priv->list, &iter, 0, name, 1, ss, -1);
    g_free (ss);
    scorelist = g_list_next (scorelist);
  }
    
  games_scores_dialog_set_hilight_private (self);
}

/* Change the currently viewed score category. There is a little bit
 * of silly-buggers here to make sure the change is temporary and
 * we end up on the right page next time. */
static void games_scores_dialog_change_category (GtkComboBox *widget, 
						 GamesScoresDialog *self)
{
  gchar *catcopy;
  gint idx;
  gchar *newcat;
  
  /* This seems like a bit of a hack, but since we're trying to
   * temporarily change the category it sort of makes sense. */

  catcopy = g_strdup (games_scores_get_category (self->_priv->scores));
  idx = gtk_combo_box_get_active (widget);
  newcat = g_hash_table_lookup (self->_priv->catindices,
				 GINT_TO_POINTER (idx));

  games_scores_set_category (self->_priv->scores, newcat);
  if (self->_priv->preservehilight) {
    self->_priv->preservehilight = FALSE;
  } else {
    self->_priv->hilight = 0;
  }
  games_scores_dialog_redraw (self);
  games_scores_set_category (self->_priv->scores, catcopy);

  g_free (catcopy);
}

/* This is to make sure we update ourselves when the window goes through
 * a hide/show cycle. */
/* FIXME: We should monitor the high scores list (or get games-scores to
 * send us a signal. */
static void games_scores_dialog_show (GamesScoresDialog *self)
{ 
  const gchar *cat;
  
  cat = games_scores_get_category (self->_priv->scores);
  if (cat)
    games_scores_dialog_set_category (self, cat);
  games_scores_dialog_redraw (self);
}

/* This is the other half of ensuring the hide/show cycle works properly. */
static void games_scores_dialog_hide (GamesScoresDialog *self) {
  self->_priv->hilight = 0;
  gtk_tree_selection_unselect_all (gtk_tree_view_get_selection (self->_priv->treeview));
}

/**
 * set_category_description:
 * @self: a pointer to a GamesScoresDialog
 * @description: A description of the categories 
 * 
 * Sets the category description label. i.e. the widget to the
 * left of the category combo box. 
 *
 **/
void games_scores_dialog_set_category_description (GamesScoresDialog *self, 
						   const gchar *description)
{
  gchar *lstr;

  lstr = g_strdup_printf ("<b>%s</b>", description);
  gtk_label_set_markup (GTK_LABEL (self->_priv->label), lstr);
  gtk_label_set_use_underline (GTK_LABEL (self->_priv->label), TRUE);
  g_free(lstr);
}

/**
 * set_message:
 * @self: a pointer to a GamesScoresDialog
 * @message: the message
 * 
 * Sets the message at the top of the dialog. Pango markup is understood.
 *
 **/
void games_scores_dialog_set_message (GamesScoresDialog *self, 
				      const gchar *message)
{
  if ((message == NULL) || (*message == '\0')) {
    gtk_widget_hide (self->_priv->message);
    gtk_widget_hide (self->_priv->hdiv);
  } else {
    gtk_widget_show (self->_priv->message);
    gtk_widget_show (self->_priv->hdiv);
    gtk_label_set_label (GTK_LABEL (self->_priv->message), message);
  }
}

/**
 * set_category_description:
 * @self: a pointer to a GamesScoresDialog
 * @pos: the position in the high score list to hilight. Should be in the
 * range 1 to 10.
 * 
 * Hilights an entry in the high score list. This is suitable for indicating
 * to the player where the game they just played is.
 *
 **/
void games_scores_dialog_set_hilight (GamesScoresDialog *self, guint pos)
{
  if ((pos < 1) || (pos > GAMES_SCORES_SIGNIFICANT))
    return;

  self->_priv->hilight = pos;
  games_scores_dialog_set_hilight_private (self);
}

/**
 * set_buttons:
 * @self: a pointer to a GamesScoresDialog
 * @buttons: An or-ed list of GamesScoresButtons
 * 
 * Changes the button sets at the buttom of the dialog
 *
 **/
void games_scores_dialog_set_buttons (GamesScoresDialog *self, guint buttons)
{
  /* Remove an existing buttons. */
  gtk_container_foreach (GTK_CONTAINER (GTK_DIALOG (self)->action_area),
                         (GtkCallback) (gtk_widget_destroy), NULL);

  /* The default is a single close button, suitable for the scores
     menu item. */
  if (buttons == 0)
	buttons = GAMES_SCORES_CLOSE_BUTTON;

  if (buttons & GAMES_SCORES_QUIT_BUTTON) {
	gtk_dialog_add_button (GTK_DIALOG (self), GTK_STOCK_QUIT,
	                       GTK_RESPONSE_REJECT);
      gtk_dialog_set_default_response (GTK_DIALOG (self), 
	       			         GTK_RESPONSE_REJECT);
  }

  if (buttons & GAMES_SCORES_UNDO_BUTTON) {
	gtk_dialog_add_button (GTK_DIALOG (self), GTK_STOCK_UNDO,
	                       GTK_RESPONSE_DELETE_EVENT);
      gtk_dialog_set_default_response (GTK_DIALOG (self), 
	       			         GTK_RESPONSE_DELETE_EVENT);
  }

  if (buttons & GAMES_SCORES_NEW_GAME_BUTTON) {
	gtk_dialog_add_button (GTK_DIALOG (self), _("New Game"),
	                       GTK_RESPONSE_ACCEPT);
      gtk_dialog_set_default_response (GTK_DIALOG (self), 
	       			         GTK_RESPONSE_ACCEPT);
  }

  if (buttons & GAMES_SCORES_CLOSE_BUTTON) {
	gtk_dialog_add_button (GTK_DIALOG (self), GTK_STOCK_CLOSE,
	                       GTK_RESPONSE_CLOSE);
      gtk_dialog_set_default_response (GTK_DIALOG (self), 
	       			         GTK_RESPONSE_CLOSE);
  }
}

static void games_scores_dialog_init (GamesScoresDialog *self) 
{
  GtkWidget *vbox;
  GtkWidget *scroll;
  GtkWidget *listview;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;

  self->_priv = G_TYPE_INSTANCE_GET_PRIVATE (self, GAMES_TYPE_SCORES_DIALOG,
					     GamesScoresDialogPrivate);

  self->_priv->style = GAMES_SCORES_STYLE_PLAIN_DESCENDING;
  /* These two hashes are the reverse of each other. As an optimisation 
   * they share the same set of strings (as keys in the first case and
   * as data in the second). The first hash is responsible for 
   * deallocating the memory for the strings. These two are only
   * valid as a pair. */
  self->_priv->categories = g_hash_table_new_full (g_str_hash, g_str_equal,
						     g_free, NULL);
  self->_priv->catindices = g_hash_table_new (g_direct_hash, g_direct_equal);
  self->_priv->catcounter = 0;
  self->_priv->hilight = 0;
  self->_priv->sethilight = -1;

  gtk_dialog_set_has_separator (GTK_DIALOG (self), FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (self), 5);
  gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (self)->vbox), 2);

  g_signal_connect (G_OBJECT (self), "show", 
		      G_CALLBACK (games_scores_dialog_show), NULL);
  g_signal_connect (G_OBJECT (self), "hide", 
		      G_CALLBACK (games_scores_dialog_hide), NULL);

  vbox = gtk_vbox_new (FALSE, 6);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);
  gtk_box_pack_end (GTK_BOX (GTK_DIALOG (self)->vbox), vbox, TRUE, TRUE, 
                    0);

  scroll = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroll),
				    GTK_POLICY_AUTOMATIC,
				    GTK_POLICY_AUTOMATIC);
  gtk_widget_set_size_request (scroll, 250, 265);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scroll),
					 GTK_SHADOW_ETCHED_IN);
  gtk_box_pack_end (GTK_BOX (vbox), scroll, TRUE, TRUE, 0);
  
  self->_priv->message = gtk_label_new ("");
  gtk_label_set_use_markup (GTK_LABEL (self->_priv->message), TRUE);
  gtk_label_set_justify (GTK_LABEL (self->_priv->message), 
	                   GTK_JUSTIFY_CENTER);    
  gtk_box_pack_start (GTK_BOX (vbox), self->_priv->message, FALSE, FALSE, 0);

  self->_priv->hdiv = gtk_hseparator_new ();
  gtk_box_pack_start (GTK_BOX (vbox), self->_priv->hdiv, FALSE, FALSE, 0);

  self->_priv->catbar = gtk_hbox_new (FALSE, 12);
  gtk_box_pack_start (GTK_BOX (vbox), self->_priv->catbar, FALSE, FALSE, 0);

  self->_priv->label = gtk_label_new (NULL);
  gtk_label_set_use_markup (GTK_LABEL (self->_priv->label), TRUE);
  gtk_box_pack_start (GTK_BOX (self->_priv->catbar), self->_priv->label,
			FALSE, FALSE, 0);	
 
  self->_priv->combo = gtk_combo_box_new_text ();
  gtk_combo_box_set_focus_on_click (GTK_COMBO_BOX (self->_priv->combo), FALSE);
  gtk_box_pack_start (GTK_BOX (self->_priv->catbar), 
			self->_priv->combo, TRUE, TRUE, 0);
  gtk_label_set_mnemonic_widget (GTK_LABEL (self->_priv->label), self->_priv->combo);

  g_signal_connect (G_OBJECT (self->_priv->combo), "changed", 
		      G_CALLBACK (games_scores_dialog_change_category), self);

  self->_priv->list = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);

  listview = gtk_tree_view_new_with_model (GTK_TREE_MODEL (self->_priv->list));
  self->_priv->treeview = GTK_TREE_VIEW (listview);
  self->_priv->cursor_handler_id = 
    g_signal_connect (G_OBJECT (self->_priv->treeview), 
			"cursor-changed", 
			G_CALLBACK (games_scores_dialog_cursor_changed), self);
  
  self->_priv->namerenderer = gtk_cell_renderer_text_new ();
  g_signal_connect (self->_priv->namerenderer, "edited", 
		      G_CALLBACK (games_scores_dialog_name_edited), self);

  self->_priv->namecolumn = gtk_tree_view_column_new_with_attributes (_("Name"),
									self->_priv->namerenderer,
									"text", 0,
									NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (listview),
				 GTK_TREE_VIEW_COLUMN (self->_priv->namecolumn));
  renderer = gtk_cell_renderer_text_new ();
  /* Note that this assumes the default style is plain. */
  column = gtk_tree_view_column_new_with_attributes (_("Score"),
						       renderer,
						       "text", 1,
						       NULL);
  g_object_set (G_OBJECT (renderer), "xalign", 1.0, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (listview),
				 GTK_TREE_VIEW_COLUMN (column));
  self->_priv->column = column;
 
  gtk_container_add (GTK_CONTAINER (scroll), listview);
  
  games_scores_dialog_set_buttons (self, GAMES_SCORES_CLOSE_BUTTON);

  gtk_window_set_destroy_with_parent (GTK_WINDOW (self), TRUE);

  gtk_widget_grab_focus (self->_priv->combo);

  gtk_widget_show_all (vbox);
  gtk_widget_hide (self->_priv->hdiv);
  gtk_widget_hide (self->_priv->message);
}

/* FIXME: There is basically no range checking. */

