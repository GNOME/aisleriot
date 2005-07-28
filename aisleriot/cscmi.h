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
#include <math.h>
#include <time.h>
#include "card.h"
#include "slot.h"


/* Init function */
void       cscm_init                          (void);

/* Scheme to C functions... */
void       add_slot                           (SCM        slot_data);
GList*     new_deck                           (SCM        deck_data);
hcard_type new_card                           (SCM        card_data);
gboolean   has_options                        (void);

/* C to Scheme functions... */
SCM        make_card                          (hcard_type card);
SCM        cscmi_start_game_lambda            (void);
SCM        cscmi_button_pressed_lambda        (SCM        slot_id,
					       SCM        cards);
SCM        cscmi_button_released_lambda       (SCM        start_slot,
					       SCM        cards,
					       SCM        end_slot);
SCM        cscmi_button_clicked_lambda        (SCM        slot_id);
SCM        cscmi_button_double_clicked_lambda (SCM        slot_id);
SCM        cscmi_droppable_lambda             (SCM        start_slot,
                                               SCM        cards,
                                               SCM        end_slot);
SCM        cscmi_game_over_lambda             (void);
SCM        cscmi_winning_game_lambda          (void);
SCM        cscmi_hint_lambda                  (void);
SCM        cscmi_get_options_lambda           (void);
SCM        cscmi_apply_options_lambda         (SCM        options);
gboolean   cscmi_timeout_lambda               (void);

#endif
