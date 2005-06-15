/* AisleRiot - cscmi.h
 * Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>

#include "cscmi.h"
#include "sol.h"
#include "slot.h"
#include "menu.h"
#include "draw.h"

/* Generic game data */
typedef struct {
  SCM start_game_lambda;
  SCM button_pressed_lambda;
  SCM button_released_lambda;
  SCM button_clicked_lambda;
  SCM button_double_clicked_lambda;
  SCM game_over_lambda;
  SCM winning_game_lambda;
  SCM hint_lambda;
  SCM get_options_lambda;
  SCM apply_options_lambda;
  SCM timeout_lambda;
  SCM droppable_lambda;
} lambda_data;

lambda_data* game_data = NULL;

/* Scheme to C functions... */

void add_slot(SCM slot_data) 
{
  hslot_type hslot = malloc(sizeof(slot_type));

  hslot->id = SCM_INUM(SCM_CAR(slot_data));
  hslot->cards = new_deck(SCM_CADR(slot_data));
  hslot->x = scm_num2dbl (SCM_CAR(SCM_CADR(SCM_CADDR(slot_data))), NULL);
  hslot->y = scm_num2dbl (SCM_CADR(SCM_CADR(SCM_CADDR(slot_data))), NULL);
  
  hslot->dx = hslot->dy = hslot->compressed_dy = 0;
  hslot->expansion_depth = 0;

  if (!strcmp("expanded", SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    hslot->compressed_dy = hslot->dy = y_expanded_offset;
  }
  else if (!strcmp("expanded-right", 
		   SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    hslot->dx = x_expanded_offset;
  }
  else if (!strcmp("partially-expanded", 
		   SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    hslot->compressed_dy = hslot->dy = y_expanded_offset;
    hslot->expansion_depth = SCM_INUM (SCM_CADDR(SCM_CADDR(slot_data)));
  }
  else if (!strcmp("partially-expanded-right", 
		   SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    hslot->dx = x_expanded_offset;
    hslot->expansion_depth = SCM_INUM(SCM_CADDR(SCM_CADDR(slot_data)));
  }

  hslot->length = hslot->exposed = 0;
  update_slot_length(hslot);

  if (slot_list)
    g_list_append(slot_list, hslot);
  else {
    slot_list = g_list_alloc();
    slot_list->data = hslot;
  }
}

hcard_type new_card(SCM card_data) 
{
  hcard_type temp_card = malloc(sizeof(card_type));

  temp_card->value = SCM_INUM(SCM_CAR(card_data));
  temp_card->suit = SCM_INUM(SCM_CADR(card_data));
  temp_card->direction = !(SCM_NFALSEP(SCM_CADDR(card_data)));

  return temp_card;
}

GList* new_deck(SCM deck_data) 
{
  SCM list_el;
  GList* temp_deck = NULL;

  if (SCM_NFALSEP(deck_data)) {
    for (list_el = deck_data; list_el != SCM_EOL; list_el = SCM_CDR(list_el))
      add_card(&temp_deck,new_card(SCM_CAR(list_el)));
  }
  
  return g_list_reverse(temp_deck);
}

/* C to Scheme functions... */
SCM make_card(hcard_type card) 
{
  return scm_cons(scm_long2num(card->value),
		 scm_cons(scm_long2num(card->suit),
			  scm_cons(SCM_BOOL(!card->direction), SCM_EOL)));
}

/* Scheme functions */
static SCM scm_gettext(SCM message)
{
  static char * input = NULL;
  char * output;

  /* This is needed because we can't free the string before returning
   * if it isn't translated. This way we have a permanent one-string
   * memory leak and nothing more. */
  if (!input)
    free (input);
  
  input = SCM_STRING_CHARS (message);
  output = _(input);
  return scm_makfrom0str (output);
}

static SCM scm_undo_set_sensitive (SCM in_state)
{
  gboolean state;

  state = SCM_NFALSEP (in_state) ? TRUE : FALSE;
  undo_set_sensitive (state);

  return SCM_EOL;
}

static SCM scm_redo_set_sensitive (SCM in_state)
{
  gboolean state;

  state = SCM_NFALSEP (in_state) ? TRUE : FALSE;
  redo_set_sensitive (state);

  return SCM_EOL;
}

static SCM scm_get_feature_word()
{
  return SCM_MAKINUM(enabled_features);
}

static SCM scm_set_feature_word(SCM features)
{
  enabled_features = SCM_INUM(features);

  droppable_is_featured = (enabled_features & DROPPABLE_FMASK);
  score_is_hidden = (enabled_features & SCOREHIDDEN_FMASK);

  return SCM_EOL;
}

static SCM scm_set_statusbar_message(SCM message)
{
  guint context_id = gtk_statusbar_get_context_id (GTK_STATUSBAR (statusbar), "message");
  gtk_statusbar_pop (GTK_STATUSBAR (statusbar), context_id);
  gtk_statusbar_push (GTK_STATUSBAR (statusbar), context_id, _(SCM_STRING_CHARS(message)));
  return SCM_EOL;
}

static SCM scm_set_surface_layout(SCM surface) 
{
  if (surface != SCM_EOL) {
    SCM list_el;
  
    delete_surface();

    for (list_el = surface; list_el != SCM_EOL; list_el = SCM_CDR(list_el))
      add_slot(SCM_CAR(list_el));
  }
  return SCM_EOL;
}


static SCM scm_reset_surface() 
{
  delete_surface();
  return SCM_EOL;
}

static SCM gg_scm_add_slot(SCM slot) 
{
  add_slot(slot);
  return SCM_EOL;
}

static SCM scm_set_slot_y_expansion(SCM scm_slot_id, SCM new_exp_val)
{
  hslot_type slot = get_slot(scm_num2int(scm_slot_id, SCM_ARG1, NULL));
  slot->compressed_dy = slot->dy = scm_num2dbl (new_exp_val, NULL);
  return SCM_EOL;
}

static SCM scm_set_slot_x_expansion(SCM scm_slot_id, SCM new_exp_val)
{
  hslot_type slot = get_slot(scm_num2int(scm_slot_id, SCM_ARG1, NULL));
  slot->dx = scm_num2dbl (new_exp_val, NULL);
  return SCM_EOL;
}

static SCM scm_get_slot(SCM scm_slot_id) 
{
  SCM cards = SCM_EOL;
  hslot_type slot = get_slot(scm_num2int(scm_slot_id, SCM_ARG1, NULL));
  GList* tempcard;
  if (slot) {
    for (tempcard = slot->cards; tempcard; tempcard = tempcard->next)
      cards = scm_cons(make_card(tempcard->data), cards);

    cards = scm_cons(scm_slot_id, scm_cons(cards, SCM_EOL));
  }
  return cards; 
}

static SCM scm_set_cards(SCM scm_slot_id, SCM new_cards) 
{
  hslot_type hslot = get_slot(scm_num2int(scm_slot_id, SCM_ARG1, NULL));
  GList* tempptr;
  
  for (tempptr = hslot->cards; tempptr; tempptr = tempptr->next)
    free(tempptr->data);

  hslot->cards = new_deck(new_cards);
  update_slot_length(hslot);

  return SCM_BOOL_T;
}

static SCM scm_set_lambda(SCM start_game_lambda, 
                          SCM pressed_lambda, 
                          SCM released_lambda, 
                          SCM clicked_lambda, 
                          SCM dbl_clicked_lambda,
                          SCM game_over_lambda,
                          SCM winning_game_lambda,
                          SCM hint_lambda,
                          SCM rest) 
{
  if (!game_data)
	 game_data = malloc(sizeof(lambda_data));
  game_data->start_game_lambda = start_game_lambda;
  game_data->button_pressed_lambda = pressed_lambda;
  game_data->button_released_lambda = released_lambda;
  game_data->button_clicked_lambda = clicked_lambda;
  game_data->button_double_clicked_lambda = dbl_clicked_lambda;
  game_data->game_over_lambda = game_over_lambda;
  game_data->winning_game_lambda = winning_game_lambda;
  game_data->hint_lambda = hint_lambda;
  game_data->get_options_lambda = SCM_CAR(rest);
  game_data->apply_options_lambda = SCM_CADR(rest);
  game_data->timeout_lambda = SCM_CADDR(rest);

  if (droppable_is_featured)
    game_data->droppable_lambda = SCM_CADDDR(rest);

  return SCM_EOL;
}

static SCM scm_myrandom(SCM range) 
{
  return scm_long2num(g_random_int_range(0,SCM_INUM(range)));
}

static SCM scm_get_score() 
{
  return scm_long2num(score);
}

static SCM scm_set_score(SCM new) 
{
  score = scm_num2int(new, SCM_ARG1, NULL);
  set_score();
  return scm_long2num(score);
}

static SCM scm_add_to_score(SCM new) 
{
  score += scm_num2int(new, SCM_ARG1, NULL);
  set_score();
  return scm_long2num(score);
}

static SCM scm_set_timeout (SCM new) 
{
  timeout = scm_num2int(new, SCM_ARG1, NULL);
  return new;
}

static SCM scm_get_timeout () 
{
  return scm_long2num(timeout);
}

void cscm_init () 
{
  scm_c_define_gsubr("set-feature-word!", 1, 0, 0, scm_set_feature_word);
  scm_c_define_gsubr("get-feature-word", 0, 0, 0, scm_get_feature_word);
  scm_c_define_gsubr("set-statusbar-message", 1, 0, 0, scm_set_statusbar_message);
  scm_c_define_gsubr("set-surface-layout", 1, 0, 0, scm_set_surface_layout);
  scm_c_define_gsubr("reset-surface", 0, 0, 0, scm_reset_surface);
  scm_c_define_gsubr("add-slot", 1, 0, 0, gg_scm_add_slot);
  scm_c_define_gsubr("get-slot", 1, 0, 0, scm_get_slot);  
  scm_c_define_gsubr("set-cards-c!", 2, 0, 0, scm_set_cards);
  scm_c_define_gsubr("set-slot-y-expansion!", 2, 0, 0, scm_set_slot_y_expansion);
  scm_c_define_gsubr("set-slot-x-expansion!", 2, 0, 0, scm_set_slot_x_expansion);
  scm_c_define_gsubr("set-lambda", 8, 0, 1, scm_set_lambda);
  scm_c_define_gsubr("random", 1, 0, 0, scm_myrandom);
  scm_c_define_gsubr("get-score", 0, 0, 0, scm_get_score);  
  scm_c_define_gsubr("set-score!", 1, 0, 0, scm_set_score);
  scm_c_define_gsubr("get-timeout", 0, 0, 0, scm_get_timeout);  
  scm_c_define_gsubr("set-timeout!", 1, 0, 0, scm_set_timeout);
  scm_c_define_gsubr("add-to-score!", 1, 0, 0, scm_add_to_score);
  scm_c_define_gsubr("_", 1, 0, 0, scm_gettext);
  scm_c_define_gsubr("undo-set-sensitive", 1, 0, 0, scm_undo_set_sensitive);
  scm_c_define_gsubr("redo-set-sensitive", 1, 0, 0, scm_redo_set_sensitive);
  eval_installed_file ("sol.scm");
}

typedef struct
{
  SCM lambda;
  int n_args;
  SCM arg1;
  SCM arg2;
  SCM arg3;
  SCM retval;
} CallData;


static SCM
cscmi_call_lambda (void *user_data)
{
  CallData *call_data = user_data;
  switch (call_data->n_args)
    {
    case 0:
      call_data->retval = scm_call_0 (call_data->lambda);
      break;
    case 1:
      call_data->retval = scm_call_1 (call_data->lambda, call_data->arg1);
      break;
    case 2:
      call_data->retval = scm_call_2 (call_data->lambda, call_data->arg1, call_data->arg2);
      break;
    case 3:
      call_data->retval = scm_call_3 (call_data->lambda, call_data->arg1, call_data->arg2, call_data->arg3);
      break;
    default:
      g_assert_not_reached ();
    }

  return SCM_EOL;
}

extern guint32 seed;
extern gchar *filename;

static void
cscmi_write_exception_details (int error_fd,
			       SCM tag,
			       SCM throw_args)
{
  char *message;
  SCM port;
  GList *slots;
  GList *slot_list;

  message = g_strdup_printf ("Variation: %s\n", filename);
  write (error_fd, message, strlen (message));
  g_free (message);

  message = g_strdup_printf ("Seed: %u\n", seed);
  write (error_fd, message, strlen (message));
  g_free (message);
  
  message = "Scheme error:\n\t";
  write (error_fd, message, strlen (message));

  port = scm_fdopen (scm_long2num (error_fd),
		     scm_mem2string ("w", sizeof (char)));
  scm_display (throw_args, port);
  scm_fsync (port);

  message = "\nScheme tag:\n\t";
  write (error_fd, message, strlen (message));
  scm_display (tag, port);
  scm_fsync (port);

  message = "\n\nDeck State:\n";
  write (error_fd, message, strlen (message));
  slots = get_slot_list ();
  if (slots)
    {
      for (slot_list = slots; slot_list; slot_list = slot_list->next)
	{
	  hslot_type slot;
	  GList *card_list;

	  slot = slot_list->data;
	  message = g_strdup_printf ("\tSlot %d\n", slot->id);
	  write (error_fd, message, strlen (message));
	  g_free (message);
	  if (slot->cards)
	    {
	      int count = 0;
	      for (card_list = slot->cards; card_list; card_list = card_list->next)
		{
		  hcard_type card = card_list->data;

		  if (count == 0)
		    message = "\t\t";
		  else
		    message = ", ";
		  write (error_fd, message, strlen (message));

		  message = g_strdup_printf ("(%d %d %s)",
					     card->suit,
					     card->value,
					     card->direction ? "#t" : "#f");
		  write (error_fd, message, strlen (message));
		  g_free (message);
		  count ++;
		  if (count == 5)
		    {
		      message = "\n";
		      write (error_fd, message, strlen (message));
		      count = 0;
		    }
		}
	      if (count != 0)
		{
		  message = "\n";
		  write (error_fd, message, strlen (message));
		}
	    }
	  else
	    {
	      message = "\t\t(Empty)\n";
	      write (error_fd, message, strlen (message));
	    }
	}
    }
  else
    {
      message = "\tNo cards in deck\n";
      write (error_fd, message, strlen (message));
    }
}

/* Called when we get an exception from guile.  We launch bug-buddy with the
 * exception information:
 */
static SCM
cscmi_catch_handler (gpointer data,
		     SCM      tag,
		     SCM      throw_args)
{
  int error_fd;
  gchar *error_file;
  GError *error = NULL;
  gchar *exec_str;

  error_fd = g_file_open_tmp ("arcrashXXXXXX",
			      &error_file,
			      &error);
  if (error)
    {
      GtkWidget *message_dialog;

      message_dialog = gtk_message_dialog_new (NULL,
					       GTK_DIALOG_DESTROY_WITH_PARENT,
					       GTK_MESSAGE_ERROR,
					       GTK_BUTTONS_OK,
					       _("A scheme exception occured and we were unable to create a temporary file to report it:\n\n%s"),
					       error->message);
      gtk_dialog_run (GTK_DIALOG (message_dialog));
      exit (1);
    }
  cscmi_write_exception_details (error_fd, tag, throw_args);
  close (error_fd);

  exec_str = g_strdup_printf ("bug-buddy --package=gnome-games --package-version=%s --appname=aisleriot --kill=%d --include=%s",
			      VERSION,
			      getpid (),
			      error_file);
  system (exec_str);
  unlink (error_file);
  exit (1);
}

SCM
cscmi_start_game_lambda (void)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->start_game_lambda;
  call_data->n_args = 0;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_button_pressed_lambda (SCM slot_id,
			     SCM cards)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->button_pressed_lambda;
  call_data->n_args = 2;
  call_data->arg1 = slot_id;
  call_data->arg2 = cards;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_button_released_lambda (SCM start_slot,
			      SCM cards,
			      SCM end_slot)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->button_released_lambda;
  call_data->n_args = 3;
  call_data->arg1 = start_slot;
  call_data->arg2 = cards;
  call_data->arg3 = end_slot;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_button_clicked_lambda (SCM slot_id)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->button_clicked_lambda;
  call_data->n_args = 1;
  call_data->arg1 = slot_id;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_button_double_clicked_lambda (SCM slot_id)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->button_double_clicked_lambda;
  call_data->n_args = 1;
  call_data->arg1 = slot_id;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_droppable_lambda (SCM start_slot,
                        SCM cards,
                        SCM end_slot)
{
  CallData *call_data;

  if (!droppable_is_featured) 
    return SCM_BOOL_F;
  
  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->droppable_lambda;
  call_data->n_args = 3;
  call_data->arg1 = start_slot;
  call_data->arg2 = cards;
  call_data->arg3 = end_slot;
  scm_internal_catch (SCM_BOOL_T,
                      cscmi_call_lambda,
                      call_data,
                      cscmi_catch_handler,
                      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_game_over_lambda (void)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->game_over_lambda;
  call_data->n_args = 0;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_winning_game_lambda (void)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->winning_game_lambda;
  call_data->n_args = 0;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_hint_lambda (void)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->hint_lambda;
  call_data->n_args = 0;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_get_options_lambda (void)
{
  CallData *call_data;
  SCM retval;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->get_options_lambda;
  call_data->n_args = 0;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  retval = call_data->retval;
  g_free (call_data);

  return retval;
}

gboolean
has_options (void)
{
  SCM options;

  options = cscmi_get_options_lambda ();
  return SCM_NFALSEP (options);
}

SCM
cscmi_apply_options_lambda (SCM options)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->apply_options_lambda;
  call_data->n_args = 1;
  call_data->arg1 = options;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}

SCM
cscmi_timeout_lambda (void)
{
  CallData *call_data;

  call_data = g_new0 (CallData, 1);
  call_data->lambda = game_data->timeout_lambda;
  call_data->n_args = 0;
  scm_internal_catch (SCM_BOOL_T,
		      cscmi_call_lambda,
		      call_data,
		      cscmi_catch_handler,
		      NULL);
  g_free (call_data);

  return call_data->retval;
}
