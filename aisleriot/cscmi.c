/* AisleRiot - cscmi.h
 * Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
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

#include "cscmi.h"
#include "sol.h"
#include "slot.h"

lambda_data* game_data = NULL;

/* Scheme to C functions... */

void add_slot(SCM slot_data) 
{
  hslot_type hslot = malloc(sizeof(slot_type));

  hslot->id = SCM_INUM(SCM_CAR(slot_data));
  hslot->cards = new_deck(SCM_CADR(slot_data));
  hslot->x = SCM_INUM(SCM_CAR(SCM_CADR(SCM_CADDR(slot_data))));
  hslot->y = SCM_INUM(SCM_CADR(SCM_CADR(SCM_CADDR(slot_data))));
  
  hslot->dx = hslot->dy = 0;
  hslot->expansion_depth = 0;

  if (!strcmp("expanded", SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    hslot->dy = y_expanded_offset;
  }
  else if (!strcmp("expanded-right", 
		   SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    hslot->dx = x_expanded_offset;
  }
  else if (!strcmp("partially-expanded", 
		   SCM_CHARS(SCM_CAR(SCM_CADDR(slot_data))))) {
    hslot->dy = y_expanded_offset;
    hslot->expansion_depth = SCM_INUM(SCM_CADDR(SCM_CADDR(slot_data)));
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
  return gh_cons(gh_long2scm(card->value),
		 gh_cons(gh_long2scm(card->suit),
			 gh_cons(gh_bool2scm(!card->direction), SCM_EOL)));
}

/* Scheme functions */
SCM scm_get_card_width() 
{
  return gh_long2scm(get_card_width());
}

SCM scm_get_card_height() 
{
  return gh_long2scm(get_card_height());
}

SCM scm_get_horiz_offset() 
{
  return gh_long2scm(get_horiz_offset());
}

SCM scm_get_vert_offset() 
{
  return gh_long2scm(get_vert_offset());
}

SCM scm_get_horiz_start() 
{
  return gh_long2scm(get_horiz_start());
}

SCM scm_get_vert_start() 
{
  return gh_long2scm(get_vert_start());
}
SCM scm_set_statusbar_message(SCM message)
{
  gnome_appbar_clear_stack (GNOME_APPBAR (GNOME_APP (app)->statusbar));
  gnome_appbar_push (GNOME_APPBAR(GNOME_APP (app)->statusbar), _(gh_scm2newstr(message,NULL)));
  return SCM_EOL;
}

SCM scm_set_surface_layout(SCM surface) 
{
  if (surface != SCM_EOL) {
    SCM list_el;
  
    delete_surface();

    for (list_el = surface; list_el != SCM_EOL; list_el = SCM_CDR(list_el))
      add_slot(SCM_CAR(list_el));
  }
  return SCM_EOL;
}


SCM scm_reset_surface() 
{
  delete_surface();
  return SCM_EOL;
}

SCM scm_add_slot(SCM slot) 
{
  add_slot(slot);
  return SCM_EOL;
}

SCM scm_get_slot(SCM scm_slot_id) 
{
  SCM cards = SCM_EOL;
  hslot_type slot = get_slot(gh_scm2int(scm_slot_id));
  GList* tempcard;
  
  if (slot) {
    for (tempcard = slot->cards; tempcard; tempcard = tempcard->next)
      cards = gh_cons(make_card(tempcard->data), cards);

    cards = gh_cons(scm_slot_id, gh_cons(cards, SCM_EOL));
  }
  return cards; 
}

SCM scm_set_cards(SCM scm_slot_id, SCM new_cards) 
{
  hslot_type hslot = get_slot(gh_scm2int(scm_slot_id));
  GList* tempptr;
  
  for (tempptr = hslot->cards; tempptr; tempptr = tempptr->next)
    free(tempptr->data);

  hslot->cards = new_deck(new_cards);
  update_slot_length(hslot);

  return SCM_BOOL_T;
}
SCM scm_set_lambda(SCM start_game_lambda, 
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
  game_data->get_options_lambda = gh_car(rest);
  game_data->apply_options_lambda = gh_cadr(rest);
  game_data->timeout_lambda = gh_caddr(rest);
  return SCM_EOL;
}

SCM scm_myrandom(SCM range) 
{
  return gh_long2scm(random()%SCM_INUM(range));
}

SCM scm_get_score() 
{
  return gh_int2scm(score);
}

SCM scm_set_score(SCM new) 
{
  score = gh_scm2int(new);
  set_score();
  return gh_int2scm(score);
}

SCM scm_add_to_score(SCM new) 
{
  score += gh_scm2int(new);
  set_score();
  return gh_int2scm(score);
}

SCM scm_set_timeout (SCM new) 
{
  timeout = gh_scm2int(new);
  return new;
}

SCM scm_get_timeout () 
{
  return gh_int2scm(timeout);
}

void cscm_init () 
{
  /* FIXME: On 1997-11-14, gh_enter stopped loading `icd-9/boot-9.scm'.
     In my copy of guile, the first define in boot-9.scm is for "provide",
     and it looked as good a test as any  */
  gh_eval_str ("(if (not (defined? \'provide))\n"
  	       "  (primitive-load-path \"ice-9/boot-9.scm\"))");
  gh_new_procedure0_0("get-card-width", scm_get_card_width);
  gh_new_procedure0_0("get-card-height", scm_get_card_height);
  gh_new_procedure0_0("get-horiz-offset",scm_get_horiz_offset);
  gh_new_procedure0_0("get-vert-offset", scm_get_vert_offset);
  gh_new_procedure0_0("get-horiz-start", scm_get_horiz_start);
  gh_new_procedure0_0("get-vert-start", scm_get_vert_start);
  gh_new_procedure1_0("set-statusbar-message", scm_set_statusbar_message);
  gh_new_procedure1_0("set-surface-layout", scm_set_surface_layout);
  gh_new_procedure0_0("reset-surface", scm_reset_surface);
  gh_new_procedure1_0("add-slot", scm_add_slot);
  gh_new_procedure1_0("get-slot", scm_get_slot);  
  gh_new_procedure2_0("set-cards-c!", scm_set_cards);
  gh_new_procedure("set-lambda", scm_set_lambda, 8, 0, 1);
  gh_new_procedure1_0("random", scm_myrandom);
  gh_new_procedure0_0("get-score", scm_get_score);  
  gh_new_procedure1_0("set-score!", scm_set_score);
  gh_new_procedure0_0("get-timeout", scm_get_timeout);  
  gh_new_procedure1_0("set-timeout!", scm_set_timeout);
  gh_new_procedure1_0("add-to-score!", scm_add_to_score);
  eval_installed_file ("sol.scm");
}
