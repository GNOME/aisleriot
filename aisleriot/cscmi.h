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

#ifndef CSCMI_H
#define CSCMI_H
#include <libguile.h>
#include <guile/gh.h>
#include <math.h>
#include <time.h>
#include "card.h"
#include "slot.h"

/* missing from guile-1.2 library!: */
#define gh_bool2scm(bool) ((SCM) (bool ? SCM_BOOL_T : SCM_BOOL_F))

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
} lambda_data;


/* variables */
extern lambda_data* game_data;

/* Init function */
void cscm_init( void );

/* Scheme to C functions... */
void add_slot(SCM);
GList* new_deck(SCM);
hcard_type new_card(SCM);

/* C to Scheme functions... */
SCM make_card(hcard_type);

#endif
