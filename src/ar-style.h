/*
 * Copyright © 2009, 2010 Christian Persch <chpe@src.gnome.org>
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

#ifndef __AR_STYLE_H__
#define __AR_STYLE_H__

#include <glib-object.h>
#include <gdk/gdk.h>

#include "ar-card-theme.h"

G_BEGIN_DECLS

#define AR_TYPE_STYLE            (ar_style_get_type())
#define AR_STYLE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_STYLE, ArStyle))
#define AR_STYLE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  AR_TYPE_STYLE, ArStyleClass))
#define AR_IS_STYLE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_STYLE))
#define AR_IS_STYLE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  AR_TYPE_STYLE))
#define AR_STYLE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  AR_TYPE_STYLE, ArStyleClass))

typedef struct _ArStyle        ArStyle;
typedef struct _ArStyleClass   ArStyleClass;
typedef struct _ArStylePrivate ArStylePrivate;

struct _ArStyle
{
  GObject parent;

  /*< private >*/
  ArStylePrivate *priv;
};

struct _ArStyleClass
{
  GObjectClass parent_class;
};

GType ar_style_get_type (void);

ArStyle* ar_style_new (void);

#define AR_STYLE_PROP_CARD_SLOT_RATIO     "card-slot-prop"
#define AR_STYLE_PROP_CARD_THEME          "card-theme"
#define AR_STYLE_PROP_CARD_OVERHANG       "card-overhang"
#define AR_STYLE_PROP_CARD_STEP           "card-step"
#define AR_STYLE_PROP_CLICK_TO_MOVE       "click-to-move"
#define AR_STYLE_PROP_DND_DRAG_THRESHOLD  "dnd-drag-threshold"
#define AR_STYLE_PROP_DOUBLE_CLICK_TIME   "double-click-time"
#define AR_STYLE_PROP_ENABLE_ANIMATIONS   "enable-animations"
#define AR_STYLE_PROP_ENABLE_SOUND        "enable-sound"
#define AR_STYLE_PROP_FOCUS_LINE_WIDTH    "focus-line-width"
#define AR_STYLE_PROP_FOCUS_PADDING       "focus-padding"
#define AR_STYLE_PROP_INTERIOR_FOCUS      "interior-focus"
#define AR_STYLE_PROP_RTL                 "rtl"
#define AR_STYLE_PROP_SELECTION_COLOR     "selection-color"
#define AR_STYLE_PROP_SHOW_TOOLTIPS       "show-tooltips"
#define AR_STYLE_PROP_SHOW_STATUS_MESSAGES "show-status-messages"
#define AR_STYLE_PROP_TOUCHSCREEN_MODE    "touchscreen-mode"

gboolean ar_style_get_enable_animations (ArStyle *style);
void     ar_style_set_enable_animations (ArStyle *style,
                                         gboolean enable);

gboolean ar_style_get_enable_sound (ArStyle *style);
void     ar_style_set_enable_sound (ArStyle *style,
                                    gboolean enable);

gboolean ar_style_get_click_to_move (ArStyle *style);
void     ar_style_set_click_to_move (ArStyle *style,
                                     gboolean enable);

ArCardTheme *ar_style_get_card_theme (ArStyle *style);
void            ar_style_set_card_theme (ArStyle *style,
                                         ArCardTheme *theme);

/* Read-only properties */
gboolean ar_style_get_touchscreen_mode (ArStyle *style);
gboolean ar_style_get_interior_focus   (ArStyle *style);
gboolean ar_style_get_rtl              (ArStyle *style);
gboolean ar_style_get_show_tooltips    (ArStyle *style);
gboolean ar_style_get_show_status_messages (ArStyle *style);

int ar_style_get_double_click_time  (ArStyle *style);
int ar_style_get_focus_line_width   (ArStyle *style);
int ar_style_get_focus_padding      (ArStyle *style);

double ar_style_get_card_slot_ratio (ArStyle *style);
double ar_style_get_card_overhang   (ArStyle *style);
double ar_style_get_card_step       (ArStyle *style);

void ar_style_get_selection_color  (ArStyle *style,
                                    GdkRGBA * const color);

gboolean ar_style_check_dnd_drag_threshold (ArStyle *style,
                                            float x1,
                                            float y1,
                                            float x2,
                                            float y2);

G_END_DECLS

#endif /* __AR_STYLE_H__ */
