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

/* C to Scheme functions... */
SCM        c2scm_card                         (hcard_type card);

gboolean   cscmi_has_options                  (void);
void       cscmi_start_game_lambda            (double *width, double *height);
gboolean   cscmi_drag_valid                   (int slot_id, GList *cards);
gboolean   cscmi_drop_valid                   (int start_slot, GList *cards,
					       int end_slot);
gboolean   cscmi_drop_cards                   (int start_slot, GList *cards,
					       int end_slot);
gboolean   cscmi_button_clicked_lambda        (int slot_id);
gboolean   cscmi_button_double_clicked_lambda (int slot_id);
gboolean   cscmi_game_over_lambda             (void);
gboolean   cscmi_winning_game_lambda          (void);
SCM        cscmi_hint_lambda                  (void);
SCM        cscmi_get_options_lambda           (void);
SCM        cscmi_apply_options_lambda         (SCM        options);
gboolean   cscmi_timeout_lambda               (void);

void cscmi_record_move(int slot_id, GList *old_cards);
void cscmi_end_move(void);
void cscmi_discard_move(void);


#endif
