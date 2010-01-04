/*
 * Copyright © 2009 Christian Persch <chpe@gnome.org>
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

#include <config.h>

#include "ar-style.h"
#include "ar-style-private.h"

enum
{
  PROP_0,
  PROP_CARD_OVERHANG,
  PROP_CARD_SLOT_RATIO,
  PROP_CARD_STEP,
  PROP_CARD_THEME,
  PROP_CLICK_TO_MOVE,
  PROP_DND_DRAG_THRESHOLD,
  PROP_DOUBLE_CLICK_TIME,
  PROP_ENABLE_ANIMATIONS,
  PROP_ENABLE_SOUND,
  PROP_FOCUS_LINE_WIDTH,
  PROP_FOCUS_PADDING,
  PROP_INTERIOR_FOCUS,
  PROP_RTL,
  PROP_SELECTION_COLOR,
  PROP_TOUCHSCREEN_MODE,
};

/* private functions */

/* GObjectClass impl */

G_DEFINE_TYPE (ArStyle, ar_style, G_TYPE_OBJECT)

static void
ar_style_init (ArStyle *style)
{
  ArStylePrivate *priv;

  priv = style->priv = G_TYPE_INSTANCE_GET_PRIVATE (style, AR_TYPE_STYLE, ArStylePrivate);

  _ar_clutter_color_from_gdk_color (&priv->selection_color, &default_selection_color);
  priv->card_slot_ratio = DEFAULT_CARD_SLOT_RATIO;
  priv->card_overhang = DEFAULT_CARD_OVERHANG;
  priv->card_step = DEFAULT_CARD_STEP;
  priv->dnd_drag_threshold = 8;
  priv->double_click_time = 250;
  priv->focus_line_width = 1;
  priv->focus_padding = 1;
  priv->enable_animations_gtk = FALSE;
  priv->enable_animations = FALSE;
  priv->enable_sound_gtk = FALSE;
  priv->enable_sound = FALSE;
  priv->touchscreen_mode = FALSE;
  priv->rtl = FALSE;
  priv->interior_focus = FALSE;
  priv->click_to_move = FALSE;
}

static void
ar_style_finalize (GObject *object)
{
  ArStyle *style = AR_STYLE (object);
  ArStylePrivate *priv = style->priv;

  if (priv->card_theme) {
    g_object_unref (priv->card_theme);
  }

  G_OBJECT_CLASS (ar_style_parent_class)->finalize (object);
}

static void
ar_style_get_property (GObject    *object,
                       guint       property_id,
                       GValue     *value,
                       GParamSpec *pspec)
{
  ArStyle *style = AR_STYLE (object);
  ArStylePrivate *priv = style->priv;

  switch (property_id) {
    case PROP_CARD_OVERHANG:
      g_value_set_double (value, ar_style_get_card_overhang (style));
      break;

    case PROP_CARD_SLOT_RATIO:
      g_value_set_double (value, ar_style_get_card_slot_ratio (style));
      break;

    case PROP_CARD_STEP:
      g_value_set_double (value, ar_style_get_card_step (style));
      break;

    case PROP_CARD_THEME:
      g_value_set_object (value, ar_style_get_card_theme (style));
      break;

    case PROP_CLICK_TO_MOVE:
      g_value_set_boolean (value, ar_style_get_click_to_move (style));
      break;

    case PROP_DND_DRAG_THRESHOLD:
      g_value_set_int (value, priv->dnd_drag_threshold);
      break;

    case PROP_DOUBLE_CLICK_TIME:
      g_value_set_int (value, ar_style_get_double_click_time (style));
      break;

    case PROP_ENABLE_ANIMATIONS:
      g_value_set_boolean (value, ar_style_get_enable_animations (style));
      break;

    case PROP_ENABLE_SOUND:
      g_value_set_boolean (value, ar_style_get_enable_sound (style));
      break;

    case PROP_FOCUS_LINE_WIDTH:
      g_value_set_int (value, ar_style_get_focus_line_width (style));
      break;

    case PROP_FOCUS_PADDING:
      g_value_set_int (value, ar_style_get_focus_padding (style));
      break;

    case PROP_INTERIOR_FOCUS:
      g_value_set_boolean (value, ar_style_get_interior_focus (style));
      break;

    case PROP_RTL:
      g_value_set_boolean (value, ar_style_get_rtl (style));
      break;

    case PROP_SELECTION_COLOR:
      g_value_set_boxed (value, &priv->selection_color);
      break;

    case PROP_TOUCHSCREEN_MODE:
      g_value_set_boolean (value, ar_style_get_touchscreen_mode (style));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_style_set_property (GObject      *object,
                       guint         property_id,
                       const GValue *value,
                       GParamSpec   *pspec)
{
  ArStyle *style = AR_STYLE (object);
  ArStylePrivate *priv = style->priv;

  switch (property_id) {
    case PROP_CARD_OVERHANG:
      priv->card_overhang = g_value_get_double (value);
      break;

    case PROP_CARD_SLOT_RATIO:
      priv->card_slot_ratio = g_value_get_double (value);
      break;

    case PROP_CARD_STEP:
      priv->card_step = g_value_get_double (value);
      break;

    case PROP_CARD_THEME:
      ar_style_set_card_theme (style, g_value_get_object (value));
      break;

    case PROP_CLICK_TO_MOVE:
      ar_style_set_click_to_move (style, g_value_get_boolean (value));
      break;

    case PROP_DND_DRAG_THRESHOLD:
      priv->dnd_drag_threshold = g_value_get_int (value);
      break;

    case PROP_DOUBLE_CLICK_TIME:
      priv->double_click_time = g_value_get_int (value);
      break;

    case PROP_ENABLE_ANIMATIONS:
      ar_style_set_enable_animations (style, g_value_get_boolean (value));
      break;

    case PROP_FOCUS_LINE_WIDTH:
      priv->focus_line_width = g_value_get_int (value);
      break;

    case PROP_FOCUS_PADDING:
      priv->focus_padding = g_value_get_int (value);
      break;

    case PROP_INTERIOR_FOCUS:
      priv->interior_focus = g_value_get_boolean (value) != FALSE;
      break;

    case PROP_RTL:
      priv->rtl = g_value_get_boolean (value) != FALSE;
      break;

    case PROP_SELECTION_COLOR: {
      ClutterColor *color;

      if ((color = g_value_get_boxed (value)) != NULL) {
        priv->selection_color = *color;
      } else {
        _ar_clutter_color_from_gdk_color (&priv->selection_color, &default_selection_color);
      }
      break;
    }

    case PROP_ENABLE_SOUND:
      ar_style_set_enable_sound (style, g_value_get_boolean (value));
      break;

    case PROP_TOUCHSCREEN_MODE:
      priv->touchscreen_mode = g_value_get_boolean (value) != FALSE;
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_style_class_init (ArStyleClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  ClutterColor color;

  g_type_class_add_private (klass, sizeof (ArStylePrivate));

  object_class->set_property = ar_style_set_property;
  object_class->get_property = ar_style_get_property;
  object_class->finalize     = ar_style_finalize;

  g_object_class_install_property
    (object_class,
     PROP_CARD_SLOT_RATIO,
     g_param_spec_double (AR_STYLE_PROP_CARD_SLOT_RATIO, NULL, NULL,
                          0.1, 1.0, DEFAULT_CARD_SLOT_RATIO,
                          G_PARAM_READWRITE |
                          G_PARAM_STATIC_STRINGS));

  /**
   * ArStyle:card-overhang:
   *
   * This controls how much of a card is allowed to hang off of the bottom
   * of the screen. If set to %0.0, the last card is always fully visible.
   */
  g_object_class_install_property
    (object_class,
     PROP_CARD_OVERHANG,
     g_param_spec_double (AR_STYLE_PROP_CARD_OVERHANG, NULL, NULL,
                          0.0, 1.0, DEFAULT_CARD_OVERHANG,
                          G_PARAM_READWRITE |
                          G_PARAM_STATIC_STRINGS));

  /**
   * ArStyle:card-step:
   *
   * This controls how much one card is offset the previous one in card stacks.
   */
  g_object_class_install_property
    (object_class,
     PROP_CARD_STEP,
     g_param_spec_double (AR_STYLE_PROP_CARD_STEP, NULL, NULL,
                          /* FIXMEchpe: allow values > 1.0 here? */
                          0.0, 1.0, DEFAULT_CARD_STEP,
                          G_PARAM_READWRITE |
                          G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_CARD_THEME,
     g_param_spec_object (AR_STYLE_PROP_CARD_THEME, NULL, NULL,
                          GAMES_TYPE_CARD_THEME,
                          G_PARAM_READWRITE |
                          G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_CLICK_TO_MOVE,
     g_param_spec_boolean (AR_STYLE_PROP_CLICK_TO_MOVE, NULL, NULL,
                           FALSE,
                           G_PARAM_READWRITE |
                           G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_DND_DRAG_THRESHOLD,
     g_param_spec_int (AR_STYLE_PROP_DND_DRAG_THRESHOLD, NULL, NULL,
                       1, G_MAXINT, 8,
                       G_PARAM_READWRITE |
                       G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_DOUBLE_CLICK_TIME,
     g_param_spec_int (AR_STYLE_PROP_DOUBLE_CLICK_TIME, NULL, NULL,
                       0, G_MAXINT, 250,
                       G_PARAM_READWRITE |
                       G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_ENABLE_ANIMATIONS,
     g_param_spec_boolean (AR_STYLE_PROP_ENABLE_ANIMATIONS, NULL, NULL,
                           FALSE,
                           G_PARAM_READWRITE |
                           G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_ENABLE_SOUND,
     g_param_spec_boolean (AR_STYLE_PROP_ENABLE_SOUND, NULL, NULL,
                           FALSE,
                           G_PARAM_READWRITE |
                           G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_FOCUS_LINE_WIDTH,
     g_param_spec_int (AR_STYLE_PROP_FOCUS_LINE_WIDTH, NULL, NULL,
                       0, G_MAXINT, 1,
                       G_PARAM_READWRITE |
                       G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_FOCUS_PADDING,
     g_param_spec_int (AR_STYLE_PROP_FOCUS_PADDING, NULL, NULL,
                       0, G_MAXINT, 1,
                       G_PARAM_READWRITE |
                       G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_INTERIOR_FOCUS,
     g_param_spec_boolean (AR_STYLE_PROP_INTERIOR_FOCUS, NULL, NULL,
                           FALSE,
                           G_PARAM_READWRITE |
                           G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_RTL,
     g_param_spec_boolean (AR_STYLE_PROP_RTL, NULL, NULL,
                           FALSE,
                           G_PARAM_READWRITE |
                           G_PARAM_STATIC_STRINGS));

  _ar_clutter_color_from_gdk_color (&color, &default_selection_color);
  g_object_class_install_property
    (object_class,
     PROP_SELECTION_COLOR,
     clutter_param_spec_color (AR_STYLE_PROP_SELECTION_COLOR, NULL, NULL,
                               &color,
                               G_PARAM_READWRITE |
                               G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_TOUCHSCREEN_MODE,
     g_param_spec_boolean (AR_STYLE_PROP_TOUCHSCREEN_MODE, NULL, NULL,
                           FALSE,
                           G_PARAM_READWRITE |
                           G_PARAM_STATIC_STRINGS));
}

/* private API */

void
_ar_clutter_color_from_gdk_color (ClutterColor *clutter_color,
                                  const GdkColor *gdk_color)
{
  clutter_color->red   = gdk_color->red   >> 8;
  clutter_color->green = gdk_color->green >> 8;
  clutter_color->blue  = gdk_color->blue  >> 8;
  clutter_color->alpha = 0xff;
}

/* public API */

/**
 * ar_style_new:
 *
 * Return value:
 */
ArStyle*
ar_style_new (void)
{
  return g_object_new (AR_TYPE_STYLE, NULL);
}

/**
 * ar_style_get_enable_animations:
 * @style: an #ArStyle
 *
 * Returns: whether animations are enabled
 */
gboolean
ar_style_get_enable_animations (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->enable_animations && priv->enable_animations_gtk;
}

/**
 * ar_style_set_enable_animations:
 * @style: an #ArStyle
 * @enable: whether to enable animations
 *
 * Note that animations are only used when this the
 * global gtk-enable-animations setting is enabled as well.
 */
void
ar_style_set_enable_animations (ArStyle *style,
                                gboolean enable)
{
  ArStylePrivate *priv = style->priv;

  enable = enable != FALSE;
  if (priv->enable_animations == enable)
    return;

  priv->enable_animations = enable;
  g_object_notify (G_OBJECT (style), AR_STYLE_PROP_ENABLE_ANIMATIONS);
}

/**
 * ar_style_get_enable_sound:
 * @style: an #ArStyle
 *
 * Returns: whether sound is enabled
 */
gboolean
ar_style_get_enable_sound (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->enable_sound && priv->enable_sound_gtk;
}

/**
 * ar_style_set_enable_sound:
 * @style: an #ArStyle
 * @enable: whether to enable sound
 *
 * Note that sound is only used when this the
 * global gtk-enable-event-sounds setting is enabled as well.
 */
void
ar_style_set_enable_sound (ArStyle *style,
                           gboolean enable)
{
  ArStylePrivate *priv = style->priv;

  enable = enable != FALSE;
  if (priv->enable_sound == enable)
    return;

  priv->enable_sound = enable;
  g_object_notify (G_OBJECT (style), AR_STYLE_PROP_ENABLE_SOUND);
}

/**
 * ar_style_get_click_to_move:
 * @style: an #ArStyle
 *
 * Returns: whether sound is enabled
 */
gboolean
ar_style_get_click_to_move (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->click_to_move;
}

/**
 * ar_style_set_click_to_move:
 * @style: an #ArStyle
 * @enable: whether to enable sound
 *
 * Note that sound is only used when this the
 * global gtk-enable-event-sounds setting is enabled as well.
 */
void
ar_style_set_click_to_move (ArStyle *style,
                           gboolean enable)
{
  ArStylePrivate *priv = style->priv;

  enable = enable != FALSE;
  if (priv->click_to_move == enable)
    return;

  priv->click_to_move = enable;
  g_object_notify (G_OBJECT (style), AR_STYLE_PROP_CLICK_TO_MOVE);
}

/**
 * ar_style_get_card_theme:
 * @style: an #ArStyle
 *
 * Returns: @style's #GamesCardTheme
 */
GamesCardTheme *
ar_style_get_card_theme (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->card_theme;
}

/**
 * ar_style_set_card_theme:
 * @style: an #ArStyle
 * @card_theme: a #GamesCardTheme
 *
 * Note that animations are only used when this the
 * global gtk-enable-animations setting is enabled as well.
 */
void
ar_style_set_card_theme (ArStyle *style,
                         GamesCardTheme *theme)
{
  ArStylePrivate *priv = style->priv;

  if (priv->card_theme == theme)
    return;

  if (priv->card_theme != NULL) {
    g_object_unref (priv->card_theme);
  }
  priv->card_theme = g_object_ref (theme);

  g_object_notify (G_OBJECT (style), AR_STYLE_PROP_CARD_THEME);
}

/**
 * ar_style_get_touchscreen_mode:
 * @style: an #ArStyle
 *
 * Returns: whether sound is enabled
 */
gboolean
ar_style_get_touchscreen_mode (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->touchscreen_mode;
}

/**
 * ar_style_get_interior_focus:
 * @style: an #ArStyle
 *
 * Returns:
 */
gboolean
ar_style_get_interior_focus (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->interior_focus;
}


/**
 * ar_style_get_rtl:
 * @style: an #ArStyle
 *
 * Returns:
 */
gboolean
ar_style_get_rtl (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->rtl;
}

/**
 * ar_style_get_double_click_time:
 * @style: an #ArStyle
 *
 * Returns: the double click time
 */
int
ar_style_get_double_click_time (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->double_click_time;
}

/**
 * ar_style_get_focus_line_width:
 * @style: an #ArStyle
 *
 * Returns:
 */
int
ar_style_get_focus_line_width (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->focus_line_width;
}

/**
 * ar_style_get_focus_padding:
 * @style: an #ArStyle
 *
 * Returns:
 */
int ar_style_get_focus_padding (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->focus_padding;
}

/**
 * ar_style_get_card_slot_ratio:
 * @style: an #ArStyle
 *
 * Returns:
 */
double
ar_style_get_card_slot_ratio (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->card_slot_ratio;
}

/**
 * ar_style_get_card_overhang:
 * @style: an #ArStyle
 *
 * Returns:
 */
double
ar_style_get_card_overhang (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->card_overhang;
}

/**
 * ar_style_get_card_step:
 * @style: an #ArStyle
 *
 * Returns:
 */
double
ar_style_get_card_step (ArStyle *style)
{
  ArStylePrivate *priv = style->priv;

  return priv->card_step;
}

/**
 * ar_style_get_selection_color:
 * @style: an #ArStyle
 * @color: location to store the color
 *
 */
void
ar_style_get_selection_color (ArStyle *style,
                              ClutterColor * const color)
{
  ArStylePrivate *priv = style->priv;

  *color = priv->selection_color;
}

/**
 * ar_style_check_dnd_drag_threshold:
 * @style:
 * @x1:
 * @y1:
 * @x2:
 * @y2:
 *
 * Checks whether the distance between (x1, y1) and (x2, y2) is
 * greater than the drag threshold.
 *
 * Returns: %TRUE if the distance between the points is greater
 *   than the drag threshold
 */
gboolean
ar_style_check_dnd_drag_threshold (ArStyle *style,
                                   float x1,
                                   float y1,
                                   float x2,
                                   float y2)
{
  ArStylePrivate *priv = style->priv;

  /* FIXMEchpe: are these coordinates pixels, or something else? */
  /* FIXMEchpe: shouldn't this be (x2 - x1)**2 + (y2 - y1)**2 >= threshold**2 ? */
  return (ABS (x2 - x1) > priv->dnd_drag_threshold ||
          ABS (y2 - y1) > priv->dnd_drag_threshold);
}
