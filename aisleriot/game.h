/*
 *  Copyright © 2007 Christian Persch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope game it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef AISLERIOT_GAME_H
#define AISLERIOT_GAME_H

#include <libguile.h>

#include <gdk/gdktypes.h>

#include <libgames-support/games-card-common.h>

G_BEGIN_DECLS

/* A slot */

typedef struct {
  int id;

  GByteArray *cards;

  /* the topmost |exposed| cards are shown on the pile */
  guint exposed;
  int expansion_depth;

  /* The location in slot co-ordinates. Filled in by the game code. */
  double x;
  double y;

  /* In-slot card spacing */
  union {
    double dx;
    double dy;
  } expansion;

  int pixeldx;
  int pixeldy;

  /* The location in pixel units. Filled in by the scaling code. */
  GdkRectangle rect;

  /* GdkPixbuf* or GdkPixmap*, no reference owned */
  GPtrArray *card_images;

  guint expanded_right : 1;
  guint expanded_down : 1;
  guint dx_set : 1;
  guint dy_set : 1;
  guint needs_update : 1;
} Slot;

#define SLOT_CARDS_N_PREALLOC (32)

/* The game */

#define AISLERIOT_TYPE_GAME         (aisleriot_game_get_type ())
#define AISLERIOT_GAME(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), AISLERIOT_TYPE_GAME, AisleriotGame))
#define AISLERIOT_GAME_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), AISLERIOT_TYPE_GAME, AisleriotGameClass))
#define AISLERIOT_IS_GAME(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), AISLERIOT_TYPE_GAME))
#define AISLERIOT_IS_GAME_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), AISLERIOT_TYPE_GAME))
#define AISLERIOT_GAME_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), AISLERIOT_TYPE_GAME, AisleriotGameClass))

#define DEFAULT_VARIATION  "klondike.scm"
#define FREECELL_VARIATION "freecell.scm"

#define AISLERIOT_GAME_ERROR  (aisleriot_game_error_quark ())

typedef struct _AisleriotGame AisleriotGame;
typedef GObjectClass AisleriotGameClass;

enum {
  GAME_ERROR_GENERIC = 0,
  GAME_ERROR_FALLBACK = 1
};

typedef enum {
  FEATURE_DROPPABLE     = 1 << 0,
  FEATURE_SCORE_HIDDEN  = 1 << 1,
  FEATURE_DEALABLE      = 1 << 2,
  ALL_FEATURES          = 0x7
} AisleriotGameFeatures;

typedef enum {
  GAME_UNINITIALISED,
  GAME_LOADED,
  GAME_BEGIN,
  GAME_RUNNING,
  GAME_OVER,
  GAME_WON,
  LAST_GAME_STATE
} AisleriotGameState;

GQuark aisleriot_game_error_quark (void);

GType aisleriot_game_get_type (void);

AisleriotGame *aisleriot_game_new (void);

GPtrArray *aisleriot_game_get_slots (AisleriotGame * game);

void aisleriot_game_slot_add_cards (AisleriotGame * game,
                                    Slot * slot,
                                    guint8 * cards, guint n_cards);

guint aisleriot_game_get_state (AisleriotGame * game);

guint aisleriot_game_get_features (AisleriotGame *game);

void aisleriot_game_start (AisleriotGame * game);

void aisleriot_game_set_paused (AisleriotGame * game, gboolean paused);

void aisleriot_game_get_geometry (AisleriotGame * game,
                                  double *width, double *height);

void aisleriot_game_undo_move (AisleriotGame * game);

void aisleriot_game_redo_move (AisleriotGame * game);

gboolean aisleriot_game_load_game (AisleriotGame * game,
                                   const char *filename, GError ** error);

void aisleriot_game_new_game (AisleriotGame * game, guint * seed);

void aisleriot_game_restart_game (AisleriotGame * game);

const char *aisleriot_game_get_game_file (AisleriotGame * game);

char *aisleriot_game_get_name (AisleriotGame * game);

guint aisleriot_game_get_seed (AisleriotGame * game);

gboolean aisleriot_game_drag_valid (AisleriotGame * game,
                                    int slot_id,
                                    guint8 * cards, guint n_cards);

gboolean aisleriot_game_drop_valid (AisleriotGame * game,
                                    int start_slot,
                                    int end_slot,
                                    guint8 * cards, guint n_cards);

gboolean aisleriot_game_drop_cards (AisleriotGame * game,
                                    int start_slot,
                                    int end_slot,
                                    guint8 * cards, guint n_cards);

gboolean aisleriot_game_button_clicked_lambda (AisleriotGame * game,
                                               int slot_id);

gboolean aisleriot_game_button_double_clicked_lambda (AisleriotGame * game,
                                                      int slot_id);

SCM aisleriot_game_hint_lambda (AisleriotGame * game);

SCM aisleriot_game_get_options_lambda (AisleriotGame * game);

SCM aisleriot_game_apply_options_lambda (AisleriotGame * game, SCM options);

gboolean aisleriot_game_timeout_lambda (AisleriotGame * game);

void aisleriot_game_record_move (AisleriotGame * game,
                                 int slot_id, guint8 * cards, guint n_cards);

void aisleriot_game_end_move (AisleriotGame * game);

void aisleriot_game_discard_move (AisleriotGame * game);

void aisleriot_game_set_click_to_move (AisleriotGame * game,
                                       gboolean enabled);

void aisleriot_game_test_end_of_game (AisleriotGame * game);

void aisleriot_game_generate_exception (AisleriotGame * game);

void aisleriot_game_deal_cards (AisleriotGame *game);

G_END_DECLS

#endif /* !AISLERIOT_GAME_H */
