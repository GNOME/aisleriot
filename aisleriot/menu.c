/* Aisleriot - menu.c
 * Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
 *
 * This game is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define MENU_C
#include <config.h>
#include <dirent.h>
#include <guile/gh.h>
#include "sol.h"
#include "menu.h"
#include "draw.h"
#include "cscmi.h"
#include "dialog.h"
#include "pixmaps/tb-xpms.h"
/*
 * Menu stuff...
 */


/* Call backs... */

int file_quit_callback (GtkWidget *app, void *data )
{
  gtk_widget_destroy (app);
  gtk_main_quit ();
  
  return TRUE;
}
int game_hint_callback (GtkWidget *app, void *data)
{
  SCM hint;
  int hint_type; 
  char hint_message[100]; //GString appears to be broken?  hack for now...
  char* temp_string;

  /* check to see if the game is over... */
  hint = gh_apply(game_data->game_over_lambda, gh_cons(SCM_EOL,SCM_EOL));

  if (!gh_scm2bool(hint)) {
	 show_hint_dialog(_("The game is over.\nNo hints are available"));
	 return TRUE;
  }  

  /* get the hint... */
  hint = gh_apply(game_data->hint_lambda, gh_cons(SCM_EOL,SCM_EOL));

  if (!gh_scm2bool(hint)) {
	 show_hint_dialog(_("This game does not have hint support yet."));
	 return TRUE;
  }
  hint_type = (gh_scm2int(gh_car(hint)));
  if (hint_type == 0) {
	 temp_string = gh_scm2newstr(gh_cadr(hint),NULL);
	 /* 'A' (not recommended, as i18n is less likely to work... */
	 strcpy(hint_message, temp_string);
	 show_hint_dialog(_((char*) hint_message));
	 free (temp_string);
  }
  else if (hint_type == 1) {
	 int offset = strlen(_("Move the "));
	 /* Move the 'A' on the 'B' */
	 temp_string = gh_scm2newstr(gh_cadr(hint),NULL);
	 strcpy(hint_message, _("Move the "));
	 strcpy(hint_message + offset, temp_string);
	 offset += strlen(temp_string);
	 free (temp_string);

	 temp_string = gh_scm2newstr(gh_caddr(hint),NULL);
	 strcpy(hint_message+offset, _(" on the "));
	 offset += strlen(_(" on the "));
	 strcpy(hint_message+offset, temp_string);
	 offset += strlen(temp_string);
	 free (temp_string);
	 strcpy(hint_message+offset, _("."));
	 show_hint_dialog(_((char*) hint_message));
  }
  else if (hint_type == 2) {
	 int offset = strlen(_("Move the "));
	 /* Move the 'A' on 'B' */
	 temp_string = gh_scm2newstr(gh_cadr(hint),NULL);
	 strcpy(hint_message, _("Move the "));
	 strcpy(hint_message + offset, temp_string);
	 offset += strlen(temp_string);
	 free (temp_string);

	 temp_string = gh_scm2newstr(gh_caddr(hint),NULL);
	 strcpy(hint_message+offset, _(" on "));
	 offset += strlen(_(" on "));
	 strcpy(hint_message+offset, temp_string);
	 offset += strlen(temp_string);
	 free (temp_string);
	 strcpy(hint_message+offset, _("."));
	 show_hint_dialog(_((char*) hint_message));
  }
  else if (hint_type == 3) {
	 /* Deal a new card <from slot A> */
  }
  else {
	 show_hint_dialog(_("This game is unable to provide a hint."));
  }
  return TRUE;
}

int game_load_game_callback (GtkWidget *app, void *file )
{
  eval_installed_file((char*) file);
  game_file = file;
  game_name = game_file_to_name((char*) file);

  return file_new_game_callback (app, NULL);
}

int file_new_game_callback (GtkWidget *app, void *data )
{
  SCM size;
  gint old_w, old_h, min_w, min_h;

  hide_game_over_box();
  hide_select_box();

  seed = random();
  score = 0;
  set_score();

  if(surface) 
    timer_start();

  size = gh_apply(game_data->start_game_lambda, SCM_EOL);
  min_w = gh_scm2int(gh_car(size))*get_horiz_offset() + 2*get_horiz_start();
  min_h = gh_scm2int(gh_cadr(size))*get_vert_offset() + 2*get_vert_start();

  gdk_window_get_size(playing_area->window, &old_w, &old_h);
  gtk_widget_set_usize (playing_area, min_w, min_h);

  if(old_w >= min_w && old_h >= min_h)
    refresh_screen();

  make_title();
  return TRUE;
}

int file_select_game_callback (GtkWidget *app, void *data )
{
  show_select_game_dialog();
  return TRUE;
}
int undo_callback (GtkWidget *app, void *data)
{
  return TRUE;
}
int redo_callback (GtkWidget *app, void *data)
{
  return TRUE;
}
int restart_callback (GtkWidget *app, void *data)
{
  srandom(seed);

  gh_apply(game_data->start_game_lambda, SCM_EOL);
  refresh_screen();
  return TRUE;
}

void help_about_callback (GtkWidget *widget, void *data)
{
  GtkWidget *about;
  const gchar *authors[] = {
	  "Main program:  Jonathan Blandford (jrb@mit.edu)",
	  "Card Games:    Jonathan Blandford (jrb@mit.edu)",
	  "                      Ryu Changwoo (cwryu@eve.kaist.ac.kr)",
	  "                      Rosanna Yuen (rwsy@mit.edu)",
          NULL
          };

  about = gnome_about_new ( _("GNOME Solitaire"), VERSION,
        		/* copyright notice */
                        "(C) 1998 Jonathan Blandford (jrb@MIT.EDU)",
                        (const char **)authors,
                        /* another comments */
                        _("The GNOME Generic Solitaire provides a rule-based "
			  "solitaire engine that allows many different games to be played"),
                        NULL);
  gtk_widget_show (about);

  return;
}

/* We fill the items in at runtime... */
GnomeUIInfo variation_sub_menu[] = {
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL, NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
};

GnomeUIInfo variation_menu[] = {
  {GNOME_APP_UI_SUBTREE, N_("Game Type"), NULL, variation_sub_menu, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  {GNOME_APP_UI_ITEM, N_("Game Seed..."), NULL, file_select_game_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN, 'n', GDK_CONTROL_MASK, NULL},
  {GNOME_APP_UI_ITEM, N_("Hint"), NULL, game_hint_callback, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 'h', GDK_CONTROL_MASK, NULL},
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL, NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
};
GnomeUIInfo help_menu[] = {
  {GNOME_APP_UI_ITEM, N_("Help with Aisleriot..."), NULL, help_about_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_ABOUT, 0, 0, NULL},
  {GNOME_APP_UI_ITEM, N_("Help with Klondike..."), NULL, help_about_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_ABOUT, 0, 0, NULL},
  {GNOME_APP_UI_SEPARATOR},
  {GNOME_APP_UI_ITEM, N_("About..."), NULL, help_about_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_ABOUT, 0, 0, NULL},
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL, NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
};

GnomeUIInfo game_menu[] = {
  {GNOME_APP_UI_ITEM, N_("New"), NULL, file_new_game_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_NEW, 'n', GDK_CONTROL_MASK, NULL},
  {GNOME_APP_UI_ITEM, N_("Properties..."), NULL, file_new_game_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF, 0, 0, NULL},
  {GNOME_APP_UI_ITEM, N_("Statistics..."), NULL, file_new_game_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF, 0, 0, NULL},
  {GNOME_APP_UI_SEPARATOR},
  {GNOME_APP_UI_ITEM, N_("Exit"), NULL, file_quit_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_EXIT, 0, 0, NULL},
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL, NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
};

GnomeUIInfo top_menu[] = {
  {GNOME_APP_UI_SUBTREE, N_("Game"), NULL, game_menu, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  {GNOME_APP_UI_SUBTREE, N_("Variation"), NULL, variation_menu, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  {GNOME_APP_UI_SUBTREE, N_("Help"), NULL, help_menu, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
  {GNOME_APP_UI_ENDOFINFO, NULL, NULL, NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL},
};
GnomeUIInfo toolbar[] =
{
  {GNOME_APP_UI_ITEM, N_("Restart"), N_("Start this game over."),
   restart_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_REFRESH, 0, 0, NULL},
  
  {GNOME_APP_UI_ITEM, N_("Seed"), N_("Start a new game after choosing the seed."),
   file_select_game_callback, NULL, NULL,
   GNOME_APP_PIXMAP_DATA, tb_new_seed_xpm, 0, 0, NULL},

  {GNOME_APP_UI_ITEM, N_("Undo"), N_("Undo the last move."),
   undo_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_UNDO, 0, 0, NULL},

  {GNOME_APP_UI_ITEM, N_("Redo"), N_("Redo the last move"),
   redo_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_REDO, 0, 0, NULL},

  {GNOME_APP_UI_ITEM, N_("Exit"), N_("Quit Aisleriot"),
   file_quit_callback, NULL, NULL,
   GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_EXIT, 0, 0, NULL},

  {GNOME_APP_UI_ENDOFINFO, NULL, NULL,
   NULL, NULL, NULL,
   GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL}
};

void create_menus(GnomeApp *app)
{
  int i;
  GtkWidget *w;
  gnome_app_create_menus(app, top_menu);
  gnome_app_create_toolbar(app, toolbar);

  for(i = 0; i < n_games; i++) {
    w = gtk_menu_item_new_with_label 
      (game_file_to_name (game_dents[i]->d_name));
    gtk_widget_show(w);
    gtk_menu_shell_append (GTK_MENU_SHELL(variation_sub_menu[0].widget), w);
    gtk_signal_connect (GTK_OBJECT(w), "activate", 
			(GtkSignalFunc) game_load_game_callback,
			(gpointer) game_dents[i]->d_name);
  }
}
