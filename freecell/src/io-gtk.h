/* io-gtk.h
   Copyright (C) 1997 Changwoo Ryu

   This program is free software; you can redistribute it and'or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Changwoo Ryu <cwryu@eve.kaist.ac.kr>. */

#ifndef __FREECELL_IO_GTK_H
#define __FREECELL_IO_GTK_H

#include <gtk/gtk.h>

#include "card.h"
#include "game.h"

/* IO interface.  */

extern GtkWidget *main_window;

void io_gtk_init (void);
void io_gtk_loop (void);

void callback_new (GtkWidget *widget, GdkEvent *event);
void callback_score (GtkWidget *widget, GdkEvent *event);
void callback_undo (GtkWidget *widget, GdkEvent *event);
void callback_option (GtkWidget *widget, GdkEvent *event);
void callback_exit (GtkWidget *widget, GdkEvent *event);
void callback_rule (GtkWidget *widget, GdkEvent *event);
void callback_about (GtkWidget *widget, GdkEvent *event);

#endif /* __FREECELL_IO_GTK_H */






