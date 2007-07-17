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

#ifndef GAMES_SCORES_DIALOG_PRIVATE_H
#define GAMES_SCORES_DIALOG_PRIVATE_H

G_BEGIN_DECLS

struct _GamesScoresDialogPrivate {
  GtkWidget *message;
  GtkWidget *hdiv;
  GtkWidget *combo;
  GtkWidget *label;
  GtkWidget *catbar;
  GtkListStore *list;
  GtkTreeView *treeview;
  GtkCellRenderer *namerenderer;
  GtkTreeViewColumn *column;
  GtkTreeViewColumn *namecolumn;
  GamesScores *scores;
  GHashTable *categories;
  GHashTable *catindices;
  gint catcounter;
  gint hilight;
  gint sethilight;
  gboolean preservehilight;
  gulong cursor_handler_id;

  /* FIXME: This should be a property. */
  gint style;
};


G_END_DECLS
#endif
