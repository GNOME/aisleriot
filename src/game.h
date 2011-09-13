/*
 * Copyright © 2007 Christian Persch
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef AISLERIOT_GAME_H
#define AISLERIOT_GAME_H

#include <gdk/gdk.h>

#include "ar-card.h"

G_BEGIN_DECLS

/* A slot */

typedef enum {
  AR_SLOT_UNKNOWN,
  AR_SLOT_CHOOSER,
  AR_SLOT_FOUNDATION,
  AR_SLOT_RESERVE,
  AR_SLOT_STOCK,
  AR_SLOT_TABLEAU,
  AR_SLOT_WASTE
} ArSlotType;

typedef struct {
  int id;
  ArSlotType type;

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
} ArSlot;

#define SLOT_CARDS_N_PREALLOC (32)

ArSlotType ar_slot_get_slot_type (ArSlot *slot);

const char *ar_slot_get_type_string (ArSlot *slot);

char *ar_slot_get_hint_string (ArSlot *slot,
                               int cardid);

/* The game */

#define AISLERIOT_TYPE_GAME         (aisleriot_game_get_type ())
#define AISLERIOT_GAME(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), AISLERIOT_TYPE_GAME, AisleriotGame))
#define AISLERIOT_GAME_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), AISLERIOT_TYPE_GAME, AisleriotGameClass))
#define AISLERIOT_IS_GAME(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), AISLERIOT_TYPE_GAME))
#define AISLERIOT_IS_GAME_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), AISLERIOT_TYPE_GAME))
#define AISLERIOT_GAME_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), AISLERIOT_TYPE_GAME, AisleriotGameClass))

#define DEFAULT_VARIATION  "klondike"
#define FREECELL_VARIATION "freecell"

#define AISLERIOT_GAME_ERROR  (aisleriot_game_error_quark ())

#define AISLERIOT_GAME_OPTIONS_MAX (0x7FFFFFFF) /* 31 bits, since we're using int not guint */

typedef struct _AisleriotGame AisleriotGame;
typedef struct _AisleriotGameClass AisleriotGameClass;

enum {
  GAME_ERROR_EXCEPTION = 0,
  GAME_ERROR_GENERIC   = 1
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

typedef enum {
  AISLERIOT_GAME_OPTION_CHECK,
  AISLERIOT_GAME_OPTION_RADIO
} AisleriotGameOptionType;

typedef struct {
  char *display_name;
  AisleriotGameOptionType type;
  guint32 value; /* exactly 1 bit set */
  gboolean set;
} AisleriotGameOption;

GQuark aisleriot_game_error_quark (void);

void aisleriot_game_option_free (AisleriotGameOption *option);

GType aisleriot_game_get_type (void);

AisleriotGame *aisleriot_game_new (void);

GPtrArray *aisleriot_game_get_slots (AisleriotGame * game);

void aisleriot_game_slot_add_cards (AisleriotGame * game,
                                    ArSlot * slot,
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

void aisleriot_game_new_game (AisleriotGame *game);

void aisleriot_game_new_game_with_rand (AisleriotGame *game,
                                        GRand *rand);

void aisleriot_game_restart_game (AisleriotGame * game);

const char *aisleriot_game_get_game_module (AisleriotGame * game);

char *aisleriot_game_get_name (AisleriotGame * game);

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

char *aisleriot_game_get_hint (AisleriotGame *game);

GList *aisleriot_game_get_options (AisleriotGame * game);

guint32 aisleriot_game_change_options (AisleriotGame *game,
                                       guint32 changed_mask,
                                       guint32 changed_value);

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

void aisleriot_game_get_card_offset (ArSlot *slot,
                                     guint card_num,
                                     gboolean old_cards,
                                     gint *xoff, gint *yoff);

void aisleriot_game_reset_old_cards (ArSlot *slot);

const char *aisleriot_game_get_score (AisleriotGame *game);

char **ar_get_game_modules (void);

G_END_DECLS

#endif /* !AISLERIOT_GAME_H */
