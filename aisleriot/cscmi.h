/* Aisleriot - cscmi.h
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

#ifndef CSCMI_H
#define CSCMI_H
#include <libguile.h>
#include <guile/gh.h>
#include <math.h>
#include <time.h>
#include "card.h"
#include "slot.h"
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
} lambda_data;


/* variables */
#ifndef CSCMI_C
extern lambda_data* game_data;
#endif


/* Scheme to C functions... */
hslot_type new_slot(SCM);
GList* new_deck(SCM);
hcard_type new_card(SCM);

/* C to Scheme functions... */
SCM make_card(hcard_type);

/* Scheme functions */
SCM scm_get_card_width();
SCM scm_get_card_height();
SCM scm_get_horiz_offset();
SCM scm_get_vert_offset();
SCM scm_get_horiz_start();
SCM scm_get_vert_start();
SCM scm_set_surface_layout(SCM);
SCM scm_reset_surface();
SCM scm_add_slot(SCM);
SCM scm_get_slot(SCM);  
SCM scm_set_cards(SCM, SCM);
SCM scm_add_to_score(SCM);
SCM scm_get_score();  
SCM scm_set_score(SCM);  

SCM scm_set_lambda(SCM, SCM, SCM, SCM, SCM, SCM, SCM, SCM);
SCM scm_random(SCM);

#endif
