/* io-gtk.c --
   Copyright (C) 1998 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
   USA */

/* Written by Changwoo Ryu <cwryu@adam.kaist.ac.kr>. */

#include <config.h>

#include <string.h>
#include <gnome.h>

#include <gtk/gtk.h>

#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>



#include "card.h"
#include "game.h"

#include "menu.h"

#include "score.h"
#include "score-dialog.h"

#include "option.h"
#include "option-dialog.h"

#include "io-gtk.h"
#include "card-draw.h"


extern int card_image_top_height(void); /* In card-draw.c */
#define FIELD_WIDTH card_image_width()
#define FIELD_HEIGHT card_image_top_height() * 14

static GtkWidget *main_window;
static GtkWidget *mb;

static GtkWidget *freecell_drawing_areas[4];
static GtkWidget *destination_drawing_areas[4];
static GtkWidget *field_drawing_areas[8];

static FREECELLGAME *freecellgame = NULL;

static enum
{
  SELECTED_NONE, SELECTED_FREECELL, SELECTED_FIELD
}
selected = SELECTED_NONE;
static gint selected_index = 0;

static gint inverted = 0;
static gint inverted_index = 0;
static gint inverted_card_index = 0;

static GdkCursor *normal_cursor;
static GdkCursor *up_cursor;

static gint stalled = 0;


static void callback_new_really_callback (GtkWidget *widget, gpointer data);

static void to_destination_auto(void);

static void update_cursors(void);

static void refresh_freecell (gint index);
static void refresh_field (gint index);
static void refresh_destination (gint index);

static void refresh_all (void);


static void callback_restart_lose (GtkWidget *widget, gpointer data);
static void callback_restart_really (void);
static void callback_new_with_lose (GtkWidget *widget, gpointer data);
static void callback_new_really (void);
static void callback_exit_with_lose (GtkWidget *widget, gpointer data);
static void callback_exit_really (void);

static void inform_invalid_move (void);

static void callback_freecell_press (GtkWidget *widget, GdkEventButton *event, gpointer client_data);
static void callback_field_press (GtkWidget *widget, GdkEventButton *event, gpointer client_data);
static void callback_field_release (GtkWidget *widget, GdkEventButton *event, gpointer client_data);
static void callback_destination_press (GtkWidget *widget, GdkEventButton *event, gpointer client_data);

static void callback_freecell_expose (GtkWidget *widget, GdkEventExpose *event, gpointer client_data);
static void callback_destination_expose (GtkWidget *widget, GdkEventExpose *event, gpointer client_data);
static void callback_field_expose (GtkWidget *widget, GdkEventExpose *event, gpointer client_data);


void
io_gtk_init (void)
{
  GtkWidget *vbox, *hbox;

  gint i;
  GtkWidget *w;

  
  /* make main window.  */
  gtk_widget_push_visual (gdk_imlib_get_visual ());
  gtk_widget_push_colormap (gdk_imlib_get_colormap ());
  main_window = gnome_app_new ("freecell", _("Freecell"));
  gtk_signal_connect (GTK_OBJECT(main_window), "delete_event",
		      GTK_SIGNAL_FUNC(callback_exit), NULL);
  gtk_widget_realize(main_window);

  card_draw_init(main_window);
  
  create_menus (GNOME_APP(main_window));

  /* vbox -- menubar, freecells & destinations, separator, fields.  */
  vbox = gtk_vbox_new (FALSE, 2);
  gnome_app_set_contents (GNOME_APP(main_window), vbox);

  hbox = gtk_hbox_new (FALSE, 2);
  for (i = 0; i < 4; i++)
    {
      freecell_drawing_areas[i] = gtk_drawing_area_new();
      gtk_signal_connect (GTK_OBJECT(freecell_drawing_areas[i]),
			  "expose_event",
			  GTK_SIGNAL_FUNC (callback_freecell_expose),
			  GINT_TO_POINTER (i));
      gtk_signal_connect (GTK_OBJECT(freecell_drawing_areas[i]),
			  "button_press_event",
			  GTK_SIGNAL_FUNC (callback_freecell_press),
			  GINT_TO_POINTER (i));
      
      gtk_drawing_area_size (GTK_DRAWING_AREA(freecell_drawing_areas[i]),
			     card_image_width(), card_image_height());
      gtk_widget_set_events (freecell_drawing_areas[i],
			     GDK_BUTTON_PRESS_MASK
			     | GDK_EXPOSURE_MASK);
      gtk_box_pack_start_defaults (GTK_BOX(hbox), freecell_drawing_areas[i]);
      gtk_widget_show(freecell_drawing_areas[i]);
    }
  w = gtk_vseparator_new();
  gtk_box_pack_start_defaults (GTK_BOX(hbox), w);
  gtk_widget_show (w);
  for (i = 0; i < 4; i++)
    {
      destination_drawing_areas[i] = gtk_drawing_area_new();
      gtk_signal_connect (GTK_OBJECT(destination_drawing_areas[i]),
			  "expose_event",
			  GTK_SIGNAL_FUNC (callback_destination_expose),
			  GINT_TO_POINTER (i));
      gtk_signal_connect (GTK_OBJECT(destination_drawing_areas[i]),
			  "button_press_event",
			  GTK_SIGNAL_FUNC (callback_destination_press),
			  GINT_TO_POINTER (i));
      gtk_drawing_area_size (GTK_DRAWING_AREA(destination_drawing_areas[i]),
			     card_image_width(), card_image_height());
      gtk_widget_set_events (destination_drawing_areas[i],
			     GDK_BUTTON_PRESS_MASK
			     | GDK_EXPOSURE_MASK);

      gtk_box_pack_start_defaults (GTK_BOX(hbox),
				   destination_drawing_areas[i]);
      gtk_widget_show(destination_drawing_areas[i]);
    }
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
  gtk_widget_show(hbox);
  
  w = gtk_hseparator_new();
  gtk_box_pack_start (GTK_BOX (vbox), w, FALSE, FALSE, 0);
  gtk_widget_show (w);
  
  hbox = gtk_hbox_new (FALSE, 2);
  for (i = 0; i < 8; i++)
    {
      field_drawing_areas[i] = gtk_drawing_area_new();
      gtk_signal_connect (GTK_OBJECT(field_drawing_areas[i]),
			  "expose_event",
			  GTK_SIGNAL_FUNC (callback_field_expose),
			  GINT_TO_POINTER (i));
      gtk_signal_connect (GTK_OBJECT(field_drawing_areas[i]),
			  "button_press_event",
			  GTK_SIGNAL_FUNC (callback_field_press),
			  GINT_TO_POINTER (i));
      gtk_signal_connect (GTK_OBJECT(field_drawing_areas[i]),
			  "button_release_event",
			  GTK_SIGNAL_FUNC (callback_field_release),
			  GINT_TO_POINTER (i));
      gtk_drawing_area_size (GTK_DRAWING_AREA(field_drawing_areas[i]),
			     FIELD_WIDTH,
			     FIELD_HEIGHT);
      gtk_widget_set_events (field_drawing_areas[i],
			     GDK_BUTTON_PRESS_MASK
			     | GDK_BUTTON_RELEASE_MASK
			     | GDK_EXPOSURE_MASK);
      
      gtk_box_pack_start_defaults (GTK_BOX(hbox), field_drawing_areas[i]);
      gtk_widget_show(field_drawing_areas[i]);
    }
  gtk_box_pack_start_defaults (GTK_BOX(vbox), hbox);
  gtk_widget_show(hbox);

  
  gtk_widget_show (vbox);
  gtk_widget_show (main_window);

  normal_cursor = gdk_cursor_new (GDK_TOP_LEFT_ARROW);
  up_cursor = gdk_cursor_new (GDK_SB_UP_ARROW);

  gtk_widget_pop_colormap ();
  gtk_widget_pop_visual ();

  stalled = 1;
}


/* Begin main I/O message loop.  */
void
io_gtk_loop (void)
{
  gtk_main();
}


/* Refresh functions.  */
static void
refresh_freecell (gint index)
{
  CARD *card;
  
  if (!freecellgame)
    return;
  
  card = freecellgame_get_freecell (freecellgame, index);

  if ((selected == SELECTED_FREECELL)
	   && selected_index == index)
    card_draw_selected_card(freecell_drawing_areas[index], card);
  else
    card_draw_card(freecell_drawing_areas[index], card);
}

static void
refresh_destination (gint index)
{
  CARD *card;
      
  if (!freecellgame)
    return;
  
  card = freecellgame_get_destination_top (freecellgame, index);
  card_draw_card(destination_drawing_areas[index], card);
}


static void
refresh_field (gint index)
{
  DECK *deck;
  
  if (!freecellgame)
    return;

  deck = freecellgame_get_field(freecellgame, index);
  
  if ((selected == SELECTED_FIELD)
      && (selected_index == index))
    {
      if (inverted && (inverted_index == index))
	card_draw_selected_deck_with_view(field_drawing_areas[index],
					       deck,
					       inverted_card_index);
      else
	card_draw_selected_deck(field_drawing_areas[index], deck);
    }
  else
    {
      if (inverted && (inverted_index == index))
	card_draw_deck_with_view(field_drawing_areas[index], deck,
				       inverted_card_index);
      else
	card_draw_deck(field_drawing_areas[index], deck);
    }
}

static void
refresh_all (void)
{
  gint i;

  for (i = 0; i < 4; i++)
    refresh_freecell(i);
  for (i = 0; i < 4; i++)
    refresh_destination(i);
  for (i = 0; i < 8; i++)
    refresh_field(i);
}


void
callback_restart (GtkWidget *widget, GdkEvent *event)
{
  if (mb)
	  return;

  if (!stalled
      && freecellgame
      && !freecellgame_is_finished(freecellgame))
    {
      mb = gnome_message_box_new (_("Exit this game?"),
				  GNOME_MESSAGE_BOX_QUESTION,
				  GNOME_STOCK_BUTTON_YES,
				  GNOME_STOCK_BUTTON_NO,
				  NULL);
/*    GTK_WINDOW(mb)->position = GTK_WIN_POS_MOUSE; */
/*    gtk_window_set_modal(GTK_WINDOW (mb), TRUE); */
      gtk_signal_connect_object(GTK_OBJECT(mb),
				"clicked",
				GTK_SIGNAL_FUNC(callback_restart_lose),
				NULL);
      gnome_dialog_set_parent (GNOME_DIALOG (mb), GTK_WINDOW (main_window));
      gtk_widget_show (mb);
    }
  else
    callback_restart_really();
}


static void
callback_restart_lose (GtkWidget *widget, gpointer data)
{
  if (GPOINTER_TO_INT (data) == 0)
    {
      score_add_lose ();
      callback_restart_really ();
    }

  mb = NULL;
}

static void
callback_restart_really (void)
{
  if (freecellgame)
    {
      freecellgame_delete (freecellgame);

      freecellgame = freecellgame_restart (4, 8);

      stalled = 0;
      selected = SELECTED_NONE;
      inverted = 0;

      refresh_all ();
      update_cursors ();
    }
}

static void
callback_cancel (GtkWidget *widget, gpointer data)
{
  gtk_widget_destroy (GTK_WIDGET(data));
}

static void
callback_seed_input (GtkWidget *widget, gpointer data)
{
  int seed;
  char buffer[64];

  seed = atoi (gtk_entry_get_text (GTK_ENTRY (data)));
  
  if (freecellgame)
    freecellgame_delete (freecellgame);

  freecellgame = freecellgame_new_with_seed (4, 8, seed);
  sprintf (buffer, _("Freecell #%d"), freecellgame->seed);
  gtk_window_set_title (GTK_WINDOW (main_window), buffer);
  

  stalled = 0;
  selected = SELECTED_NONE;
  inverted = 0;

  refresh_all();
  update_cursors();
}


static void
callback_new_with_seed_really (void)
{
  GtkWidget *dialog, *entry, *label;

  dialog = gnome_dialog_new (_("Seed"),
			     GNOME_STOCK_BUTTON_OK,
			     GNOME_STOCK_BUTTON_CANCEL, NULL);

  gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (main_window));

  label = gtk_label_new (_("Seed value:"));
  gtk_box_pack_start_defaults (GTK_BOX(GNOME_DIALOG(dialog)->vbox), label);
  gtk_widget_show (label);
  
  entry = gtk_entry_new ();
  gtk_box_pack_start_defaults (GTK_BOX(GNOME_DIALOG(dialog)->vbox), entry);
  gtk_widget_show (entry);

  gnome_dialog_button_connect (GNOME_DIALOG(dialog), 0,
			       GTK_SIGNAL_FUNC (callback_seed_input), entry);
  gnome_dialog_button_connect (GNOME_DIALOG(dialog), 0,
			       GTK_SIGNAL_FUNC (callback_cancel), dialog);
  gnome_dialog_button_connect (GNOME_DIALOG(dialog), 1,
			       GTK_SIGNAL_FUNC (callback_cancel), dialog);
  gtk_widget_show (dialog);
}


static void
callback_new_with_seed_with_lose (GtkWidget *widget, gpointer data)
{
  if ( GPOINTER_TO_INT (data) == 0)
    {
      score_add_lose();
      callback_new_with_seed_really();
    }
  mb = NULL;
}


void
callback_new_with_seed (GtkWidget *widget, GdkEvent *event)
{
  if (mb)
	  return;

  if (!stalled
      && freecellgame
      && !freecellgame_is_finished(freecellgame))
    {
      mb = gnome_message_box_new (_("Exit this game?"),
				  GNOME_MESSAGE_BOX_QUESTION,
				  GNOME_STOCK_BUTTON_YES,
				  GNOME_STOCK_BUTTON_NO,
				  NULL);
/*    GTK_WINDOW(mb)->position = GTK_WIN_POS_MOUSE; */
/*    gtk_window_set_modal(GTK_WINDOW(mb), TRUE); */
      gtk_signal_connect_object(GTK_OBJECT(mb),
				"clicked",
				GTK_SIGNAL_FUNC(callback_new_with_seed_with_lose),
				NULL);
      gnome_dialog_set_parent (GNOME_DIALOG (mb), GTK_WINDOW (main_window));
      gtk_widget_show (mb);
    }
  else
    callback_new_with_seed_really();
}

static void
callback_new_really_callback (GtkWidget *widget, gpointer data)
{
  if (GPOINTER_TO_INT (data) == 0)
    callback_new_really ();
}

static void
callback_new_really (void)
{
  char buffer[64];
  
  if (freecellgame)
    freecellgame_delete (freecellgame);
  
  freecellgame = freecellgame_new (4, 8);
  sprintf (buffer, _("Freecell #%d"), freecellgame->seed);
  gtk_window_set_title (GTK_WINDOW (main_window), buffer);

  stalled = 0;
  selected = SELECTED_NONE;
  inverted = 0;

  refresh_all();
  update_cursors();
}


static void
callback_new_with_lose (GtkWidget *widget, gpointer data)
{
  if (GPOINTER_TO_INT (data) == 0)
    {
      score_add_lose();
      callback_new_really();
    }
  mb = NULL;
}


void
callback_new (GtkWidget *widget, GdkEvent *event)
{
  if (mb)
	  return;

  if (!stalled
      && freecellgame
      && !freecellgame_is_finished(freecellgame))
    {
      mb = gnome_message_box_new (_("Exit this game?"),
				  GNOME_MESSAGE_BOX_QUESTION,
				  GNOME_STOCK_BUTTON_YES,
				  GNOME_STOCK_BUTTON_NO,
				  NULL);
/*    GTK_WINDOW(mb)->position = GTK_WIN_POS_MOUSE; */
/*    gtk_window_set_modal(GTK_WINDOW(mb), TRUE); */
      gtk_signal_connect_object(GTK_OBJECT(mb),
				"clicked",
				GTK_SIGNAL_FUNC(callback_new_with_lose),
				NULL);
      gnome_dialog_set_parent (GNOME_DIALOG (mb), GTK_WINDOW (main_window));
      gtk_widget_show (mb);
    }
  else
    callback_new_really();
  
}

void
callback_score (GtkWidget *widget, GdkEvent *event)
{
  GtkWidget *dialog;

  dialog = score_dialog(); 
}

void
callback_undo (GtkWidget *widget, GdkEvent *event)
{
  if (freecellgame && freecellgame_undo (freecellgame) > 0)
    {
      selected = SELECTED_NONE;
      inverted = 0;
      refresh_all ();
    }
}


void
callback_option (GtkWidget *widget, GdkEvent *event)
{
  GtkWidget *dialog;

  dialog = option_dialog ();

  gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (main_window));

  gtk_widget_show (dialog);
}


void
callback_rule (GtkWidget *widget, GdkEvent *event)
{
  GtkWidget *mb;

  mb = gnome_message_box_new (_("Sorry, this feature is not (yet) implemented."),
			      GNOME_MESSAGE_BOX_INFO,
			      GNOME_STOCK_BUTTON_OK,
			      NULL);
  /*GTK_WINDOW(mb)->position = GTK_WIN_POS_MOUSE;*/
  gtk_window_set_modal (GTK_WINDOW(mb), TRUE);
  gnome_dialog_set_parent (GNOME_DIALOG (mb), GTK_WINDOW (main_window));
  gtk_widget_show (mb);
}


static void
callback_exit_really (void)
{
  score_write();
  option_write();
  gtk_main_quit();
}

static void
callback_exit_with_lose (GtkWidget *widget, gpointer data)
{
  if (GPOINTER_TO_INT (data) == 0)
    {
      score_add_lose();
      callback_exit_really();
    }
  mb = NULL;
}


void
callback_exit (GtkWidget *widget, GdkEvent *event)
{
  if (mb)
	  return;

  if (!stalled
      && freecellgame
      && !freecellgame_is_finished(freecellgame))
    {
      mb = gnome_message_box_new (_("Exit this game?"),
				  GNOME_MESSAGE_BOX_QUESTION,
				  GNOME_STOCK_BUTTON_YES,
				  GNOME_STOCK_BUTTON_NO,
				  NULL);

/*    GTK_WINDOW(mb)->position = GTK_WIN_POS_MOUSE; */
/*    gtk_window_set_modal(GTK_WINDOW(mb), TRUE); */
      gtk_signal_connect_object(GTK_OBJECT(mb),
				"clicked",
				GTK_SIGNAL_FUNC(callback_exit_with_lose),
				NULL);
      gnome_dialog_set_parent (GNOME_DIALOG (mb), GTK_WINDOW (main_window));
      gtk_widget_show (mb);
    }
  else
    callback_exit_really();
}

void
callback_about (GtkWidget *widget, GdkEvent *event)
{
  GtkWidget *about;
  gchar *authors[] = {
    N_("Changwoo Ryu."),
    NULL
  };

#ifdef ENABLE_NLS
#ifndef ELEMENTS  
#define ELEMENTS(x) (sizeof(x) / sizeof(x[0]))
#endif /* ELEMENTS */
  {
    int i;

    for (i = 0; i < (ELEMENTS(authors) - 1); i++)
      authors[i] = _(authors[i]);
  }  
#endif /* ENABLE_NLS */
  
  about = gnome_about_new (_("Freecell"), VERSION,
			   "(C) 1998 Free Software Foundation, Inc.",
			   (const char **)authors,
			   _("Reimplement the popular solitaire card game."),
			   NULL);
  gtk_widget_show (about);
}

static void
inform_invalid_move (void)
{
  GtkWidget *mb;

  mb = gnome_message_box_new (_("That move is invalid."),
			      GNOME_MESSAGE_BOX_ERROR,
			      GNOME_STOCK_BUTTON_OK,
			      NULL);
  /*GTK_WINDOW(mb)->position = GTK_WIN_POS_MOUSE;*/
  gtk_window_set_modal(GTK_WINDOW(mb), TRUE);
  gnome_dialog_set_parent (GNOME_DIALOG (mb), GTK_WINDOW (main_window));
  gtk_widget_show (mb);
}

static void
callback_freecell_press (GtkWidget *widget, GdkEventButton *event,
			 gpointer client_data)
{
  gint index;

  
  if (!freecellgame || stalled)
    return;

  index = GPOINTER_TO_INT (client_data);
	
  if (event->type == GDK_BUTTON_PRESS)
    {
      switch(event->button)
	{
	case 1:
	  if (selected == SELECTED_FIELD)
	    {
	      if (freecellgame_field_to_freecell(freecellgame,
						 selected_index,
						 index) < 0)
		{
		  if (option_inform_invalid_move)
		    {
		      inform_invalid_move();
		      selected = SELECTED_NONE;
		      refresh_field(selected_index);
		    }
		}
	      else
		{
		  selected = SELECTED_NONE;
		  refresh_field(selected_index);
		  refresh_freecell(index);
		  to_destination_auto();
		}
	    }
	  else if (selected == SELECTED_FREECELL)
	    {
	      if (selected_index == index)
		{
		  selected = SELECTED_NONE;
		  refresh_freecell(index);
		}
	      else if (freecellgame_freecell_to_freecell(freecellgame,
							 selected_index,
							 index) < 0)
		{
		  if (option_inform_invalid_move)
		    {
		      inform_invalid_move();
		      selected = SELECTED_NONE;
		      refresh_freecell(selected_index);
		    }
		}
	      else
		{
		  selected = SELECTED_NONE;
		  refresh_freecell(selected_index);
		  refresh_freecell(index);
		  to_destination_auto();
		}
	    }
	  else
	    {
	      if (freecellgame_get_freecell(freecellgame, index))
		{
		  selected = SELECTED_FREECELL;
		  selected_index = index;
		  refresh_freecell(index);
		}
	    }
	  break;
      
	default:
	  if (selected == SELECTED_FREECELL)
	    {
	      selected = SELECTED_NONE;
	      refresh_freecell(selected_index);
	    }
	  else if (selected == SELECTED_FIELD)
	    {
	      selected = SELECTED_NONE;
	      refresh_field(selected_index);
	    }
	  else
	    selected = SELECTED_NONE;

	  break;
	}
    }
  update_cursors();
  return;
}

static void
callback_destination_press (GtkWidget *widget, GdkEventButton *event,
			    gpointer client_data)
{
  gint index;
  
  if (!freecellgame || stalled)
    return;

  index = GPOINTER_TO_INT (client_data);

  if (event->type == GDK_BUTTON_PRESS)
    {
      switch(event->button)
	{
	case 1:
	  if (selected == SELECTED_FREECELL)
	    {
	      if (freecellgame_freecell_to_destination(freecellgame,
						       selected_index,
						       index) < 0)
		{
		  if (option_inform_invalid_move)
		    {
		      inform_invalid_move();
		      selected = SELECTED_NONE;
		      refresh_freecell(selected_index);
		    }
		}
	      else
		{
		  selected = SELECTED_NONE;
		  refresh_freecell(selected_index);
		  refresh_destination(index);
		  to_destination_auto();
		}
	    }
	  else if (selected == SELECTED_FIELD)
	    {
	      if (freecellgame_field_to_destination(freecellgame,
						    selected_index,
						    index) < 0)
		{
		  if (option_inform_invalid_move)
		    {
		      inform_invalid_move();
		      selected = SELECTED_NONE;
		      refresh_field(selected_index);
		    }
		}
	      else
		{
		  selected = SELECTED_NONE;
		  refresh_field(selected_index);
		  refresh_destination(index);
		  to_destination_auto();
		}
	    }
	  break;

	default:
	  if (selected == SELECTED_FREECELL)
	    {
	      selected = SELECTED_NONE;
	      refresh_freecell(selected_index);
	    }
	  else if (selected == SELECTED_FIELD)
	    {
	      selected = SELECTED_NONE;
	      refresh_field(selected_index);
	    }
	  else
	    selected = SELECTED_NONE;

	  break;
	}
    }
  update_cursors();
  return;
}


static void
callback_field_press (GtkWidget *widget, GdkEventButton *event,
		      gpointer client_data)
{
  gint index;
  
  int tmp, tmp2;

  if (!freecellgame || stalled)
    return;

  index = GPOINTER_TO_INT (client_data);

  if (event->type == GDK_BUTTON_PRESS)
    {
      switch(event->button)
	{
	case 1:
	  if (selected == SELECTED_FREECELL)
	    {
	      if (freecellgame_freecell_to_field(freecellgame,
						 selected_index, index) < 0)
		{
		  if (option_inform_invalid_move)
		    {
		      inform_invalid_move();
		      selected = SELECTED_NONE;
		      refresh_freecell(selected_index);
		    }
		}
	      else
		{
		  selected = SELECTED_NONE;
		  refresh_freecell(selected_index);
		  refresh_field(index);
		  to_destination_auto();
		}
	    }
	  else if (selected == SELECTED_FIELD)
	    {
	      if (selected_index == index)
		{
		  selected = SELECTED_NONE;
		  refresh_field(selected_index);
		}
	      else
		{
		  if (option_move_one_by_one)
		    tmp = freecellgame_field_to_field (freecellgame,
						       selected_index,
						       index);
		  else
		    tmp = freecellgame_field_to_field_sequence (freecellgame,
							        selected_index,
							        index);

		  if ((tmp < 0) && option_inform_invalid_move)
		    {
			  inform_invalid_move();
			  selected = SELECTED_NONE;
			  refresh_field (selected_index);
		    }
		  else
		    {
		      selected = SELECTED_NONE;
		      refresh_field (selected_index);
		      refresh_field (index);
		      to_destination_auto ();
		    }
		}
	    }
	  else
	    {
	      if (deck_number(freecellgame_get_field(freecellgame, index))
		  != 0)
		{
		  selected = SELECTED_FIELD;
		  selected_index = index;
		  refresh_field(index);
		}
	    }
	  break;
	  
	case 3:
	  {
	    DECK *deck;
	    int n;

	    deck = freecellgame_get_field (freecellgame, index);
	    n = card_draw_get_index_from_deck(widget, event->x, event->y,
					      deck);
	    if (n >= 0)
	      {
		inverted = 1;
		inverted_index = index;
		inverted_card_index = n;
		refresh_field (index);
	      }
	    break;
	  }

	default:
	  if (selected == SELECTED_FREECELL)
	    {
	      selected = SELECTED_NONE;
	      refresh_freecell(selected_index);
	    }
	  else if (selected == SELECTED_FIELD)
	    {
	      selected = SELECTED_NONE;
	      refresh_field(selected_index);
	    }
	  else
	    selected = SELECTED_NONE;
	  break;
	}
    }
  else if (event->type == GDK_2BUTTON_PRESS)
    {
      switch (event->button)
	{
	case 1:
	  tmp = freecellgame_field_to_empty_freecell (freecellgame,
						      index, &tmp2);
	  selected = SELECTED_NONE;
	  if (tmp >= 0)
	    {
	      refresh_field(index);
	      refresh_freecell(tmp2);
	    }
	  to_destination_auto();
	default:
	  selected = SELECTED_NONE;
	  break;
	}
    }
  
  update_cursors();
  return;
}

static void
callback_field_release (GtkWidget *widget, GdkEventButton *event,
			gpointer client_data)
{
  gint index;

  
  if (!freecellgame || stalled)
    return;

  index = GPOINTER_TO_INT (client_data);

  if (event->type == GDK_BUTTON_RELEASE)
    {
      if (event->button == 3)
	inverted = 0;
      refresh_field(inverted_index);
    }
  return;
}


static void
update_cursors (void)
{
  int i, tmp;

  switch (selected)
    {
    case SELECTED_NONE:
      for (i = 0; i < 4; i++)
	gdk_window_set_cursor(freecell_drawing_areas[i]->window,
			      normal_cursor);
      for (i = 0; i < 4; i++)
	gdk_window_set_cursor(destination_drawing_areas[i]->window,
			      normal_cursor);
      for (i = 0; i < 8; i++)
	gdk_window_set_cursor(field_drawing_areas[i]->window,
			      normal_cursor);
      break;
    case SELECTED_FIELD:
      for (i = 0; i < 4; i++)
	{
	  if (freecellgame_can_move_field_to_freecell (freecellgame,
						       selected_index, i))
	    gdk_window_set_cursor(freecell_drawing_areas[i]->window,
				  up_cursor);
	  else
	    gdk_window_set_cursor(freecell_drawing_areas[i]->window,
				  normal_cursor);
	}
      for (i = 0; i < 4; i++)
	{
	  if (freecellgame_can_move_field_to_destination (freecellgame,
							  selected_index, i))
	    gdk_window_set_cursor(destination_drawing_areas[i]->window,
				  up_cursor);
	  else
	    gdk_window_set_cursor(destination_drawing_areas[i]->window,
				  normal_cursor);
	}
      for (i = 0; i < 8; i++)
	{
	  if (option_move_one_by_one)
	    tmp = freecellgame_can_move_field_to_field (freecellgame, selected_index, i);
	  else
	    tmp = freecellgame_can_move_field_to_field_sequence (freecellgame, selected_index, i);

	  if (tmp)
	    gdk_window_set_cursor(field_drawing_areas[i]->window,
				  up_cursor);
	  else
	    gdk_window_set_cursor(field_drawing_areas[i]->window,
				  normal_cursor);
	}
      break;
    case SELECTED_FREECELL:
      for (i = 0; i < 4; i++)
	{
	  if (freecellgame_can_move_freecell_to_freecell (freecellgame,
							  selected_index, i))
	    gdk_window_set_cursor(freecell_drawing_areas[i]->window,
				  up_cursor);
	  else
	    gdk_window_set_cursor(freecell_drawing_areas[i]->window,
				  normal_cursor);
	}
      for (i = 0; i < 4; i++)
	{
	  if (freecellgame_can_move_freecell_to_destination (freecellgame,
							     selected_index,
							     i))
	    gdk_window_set_cursor(destination_drawing_areas[i]->window,
				  up_cursor);
	  else
	    gdk_window_set_cursor(destination_drawing_areas[i]->window,
				  normal_cursor);
	}
      for (i = 0; i < 8; i++)
	{
	  if (freecellgame_can_move_freecell_to_field (freecellgame,
						       selected_index, i))
	    gdk_window_set_cursor(field_drawing_areas[i]->window,
				  up_cursor);
	  else
	    gdk_window_set_cursor(field_drawing_areas[i]->window,
				  normal_cursor);
	}
      break;
    }
}




static void
callback_freecell_expose (GtkWidget *widget, GdkEventExpose *event,
			  gpointer client_data)
{
  refresh_freecell(GPOINTER_TO_INT (client_data));
  return;
}

static void
callback_destination_expose (GtkWidget *widget, GdkEventExpose *event,
			     gpointer client_data)
{
  refresh_destination(GPOINTER_TO_INT (client_data));
  return;
}

static void
callback_field_expose (GtkWidget *widget, GdkEventExpose *event,
		       gpointer client_data)
{
  refresh_field(GPOINTER_TO_INT (client_data));
  return;
}



static void
to_destination_auto(void)
{
  int is_freecell, to_index, from_index;
  GtkWidget *mb;
  
  while (freecellgame_to_destination_auto(freecellgame, &is_freecell,
					  &from_index, &to_index) >= 0)
    {
      if (is_freecell)
	refresh_freecell(from_index);
      else
	refresh_field(from_index);
      refresh_destination(to_index);
    }

  if (freecellgame_is_there_no_way(freecellgame))
    {
      freecellgame_delete_history(freecellgame);

      mb = gnome_message_box_new (_("Sorry, there are no more valid moves."),
				  GNOME_MESSAGE_BOX_INFO,
				  GNOME_STOCK_BUTTON_OK,
				  NULL);

/*    GTK_WINDOW(mb)->position = GTK_WIN_POS_MOUSE; */
      gtk_window_set_modal(GTK_WINDOW(mb), TRUE);
      gnome_dialog_set_parent (GNOME_DIALOG (mb), GTK_WINDOW (main_window));
      gtk_widget_show (mb);
      score_add_lose();
      stalled = 1;
    }
  
  if (freecellgame_is_finished(freecellgame))
    {
      freecellgame_delete_history(freecellgame);

      mb = gnome_message_box_new (_("Congratulations.  You won.\nDo you want to play again?"),
				  GNOME_MESSAGE_BOX_QUESTION,
				  GNOME_STOCK_BUTTON_YES,
				  GNOME_STOCK_BUTTON_NO,
				  NULL);
/*    GTK_WINDOW(mb)->position = GTK_WIN_POS_MOUSE; */
      gtk_window_set_modal(GTK_WINDOW(mb), TRUE);
      gnome_dialog_set_parent (GNOME_DIALOG (mb), GTK_WINDOW (main_window));
      gtk_signal_connect_object (GTK_OBJECT (mb),
				 "clicked",
				 GTK_SIGNAL_FUNC(callback_new_really_callback),
				 NULL);
      gtk_widget_show (mb);
      score_add_win();
      stalled = 1;
    }
  

}

