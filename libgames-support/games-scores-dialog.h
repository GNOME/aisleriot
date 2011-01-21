/* -*- mode: C -*-

   games-scores-dialog.h
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

#ifndef GAMES_SCORES_DIALOG_H
#define GAMES_SCORES_DIALOG_H

#include <gtk/gtk.h>

#include "games-score.h" /* For GamesScoreStyle. */
#include "games-scores.h"

G_BEGIN_DECLS
#define GAMES_TYPE_SCORES_DIALOG (games_scores_dialog_get_type ())
#define GAMES_SCORES_DIALOG(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                  GAMES_TYPE_SCORES_DIALOG, \
                                  GamesScoresDialog))
#define GAMES_SCORES_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), \
                                          GAMES_TYPE_SCORES_DIALOG, \
                                          GamesScoresDialogClass))
#define GAMES_IS_SCORES_DIALOG(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                     GAMES_TYPE_SCORES_DIALOG))
#define GAMES_IS_SCORES_DIALOG_CLASS(kls) (G_TYPE_CHECK_CLASS_TYPE ((kls), \
                                           GAMES_TYPE_SCORES_DIALOG))
#define GAMES_GET_SCORES_DIALOG_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                            GAMES_TYPE_SCORES_DIALOG, \
                                            GamesScoresDialogClass))

/* This enumeration is to provide common buttons for the dialog. */

typedef enum {
  GAMES_SCORES_CLOSE_BUTTON = 1,
  GAMES_SCORES_NEW_GAME_BUTTON = 2,	
  GAMES_SCORES_UNDO_BUTTON = 4,
  GAMES_SCORES_QUIT_BUTTON = 8,
} GamesScoresButtons;

typedef struct GamesScoresDialogPrivate GamesScoresDialogPrivate;

typedef struct {
  GtkDialog dialog;
  /*< private >*/
  GamesScoresDialogPrivate *priv;
} GamesScoresDialog;

typedef struct {
  GtkDialogClass parent_class;
} GamesScoresDialogClass;

GType      games_scores_dialog_get_type                 (void);
GtkWidget *games_scores_dialog_new                      (GtkWindow *parent_window,
                                                         GamesScores *scores,
                                                         const gchar *title);
void       games_scores_dialog_set_category_description (GamesScoresDialog *self, 
                                                         const gchar *description);
void       games_scores_dialog_set_hilight              (GamesScoresDialog *self,
                                                         guint pos);
void       games_scores_dialog_set_message              (GamesScoresDialog *self, 
                                                         const gchar *message);
void       games_scores_dialog_set_buttons              (GamesScoresDialog *self,
                                                         guint buttons);

G_END_DECLS
#endif
