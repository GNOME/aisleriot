/*
 * Copyright © 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 * Copyright © 2007, 2008, 2009 Christian Persch
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

#include "ar-clutter-embed.h"

#include "ar-style.h"
#include "ar-style-private.h"

/**
 * SECTION: ar-clutter-embed
 * @short_description: a #GtkClutterEmbed derivative
 *
 * #ArClutterEmbed is a #GtkClutterEmbed derivative that syncs the
 * properties of a #ArStyle to its style properties and the #GtkSettings
 * properties of its #GdkScreen.
 */

G_DEFINE_TYPE (ArClutterEmbed, ar_clutter_embed, GTK_CLUTTER_TYPE_EMBED)

enum
{
  PROP_0,
  PROP_STYLE
};

struct _ArClutterEmbedPrivate
{
  ArStyle *style;

  GdkCursor *cursor[AR_LAST_CURSOR];
};

#define I_(string) (g_intern_static_string (string))

/* private functions */

static void
sync_settings (GtkSettings *settings,
               GParamSpec *pspec,
               ArClutterEmbed *embed)
{
  ArClutterEmbedPrivate *embed_priv = embed->priv;
  ArStyle *style = embed_priv->style;
  ArStylePrivate *style_priv = style->priv;
  GObject *style_object = G_OBJECT (style);
  const char *pspec_name;

  if (pspec)
    pspec_name = pspec->name;
  else
    pspec_name = NULL;

  g_object_freeze_notify (style_object);

  if (pspec_name == NULL || pspec_name == I_("gtk-dnd-drag-threshold")) {
    int threshold;

    g_object_get (settings, "gtk-dnd-drag-threshold", &threshold, NULL);

    if (threshold != style_priv->dnd_drag_threshold) {
      style_priv->dnd_drag_threshold = threshold;

      g_object_notify (style_object, AR_STYLE_PROP_DND_DRAG_THRESHOLD);
    }
  }

  if (pspec_name == NULL || pspec_name == I_("gtk-double-click-time")) {
    int double_click_time;

    g_object_get (settings, "gtk-double-click-time", &double_click_time, NULL);

    if (double_click_time != style_priv->double_click_time) {
      style_priv->double_click_time = double_click_time;

      g_object_notify (style_object, AR_STYLE_PROP_DOUBLE_CLICK_TIME);
    }
  }

  if (pspec_name == NULL || pspec_name == I_("gtk-enable-animations")) {
    gboolean enable;

    g_object_get (settings, "gtk-enable-animations", &enable, NULL);

    if (enable != style_priv->enable_animations_gtk) {
      style_priv->enable_animations_gtk = enable;

      /* FIXMEchpe: only notify if the effective setting changed */
      g_object_notify (style_object, AR_STYLE_PROP_ENABLE_ANIMATIONS);
    }
  }

#if GTK_CHECK_VERSION (2, 14, 0)
  if (pspec_name == NULL || pspec_name == I_("gtk-enable-event-sounds")) {
    gboolean enable;

    g_object_get (settings, "gtk-enable-event-sounds", &enable, NULL);

    if (enable != style_priv->enable_sound_gtk) {
      style_priv->enable_sound_gtk = enable;

      /* FIXMEchpe: only notify if the effective setting changed */
      g_object_notify (style_object, AR_STYLE_PROP_ENABLE_SOUND);
    }
  }
#endif /* GTK+ >= 2.14.0 */

#if GTK_CHECK_VERSION (2, 10, 0)
  if (pspec_name == NULL || pspec_name == I_("gtk-touchscreen-mode")) {
    gboolean enable;

    g_object_get (settings, "gtk-touchscreen-mode", &enable, NULL);

    if (enable != style_priv->touchscreen_mode) {
      style_priv->touchscreen_mode = enable;

      /* FIXMEchpe: only notify if the effective setting changed */
      g_object_notify (style_object, AR_STYLE_PROP_TOUCHSCREEN_MODE);
    }
  }
#endif /* GTK+ >= 2.10.0 */

  g_object_thaw_notify (style_object);
}

static void
sync_direction (ArClutterEmbed *embed,
                GtkTextDirection previous_direction)
{

  ArClutterEmbedPrivate *embed_priv = embed->priv;
  ArStyle *style = embed_priv->style;
  ArStylePrivate *style_priv = style->priv;
  GObject *style_object = G_OBJECT (style);
  GtkTextDirection direction;
  gboolean rtl;

  direction = gtk_widget_get_direction (GTK_WIDGET (embed));
  if (direction == previous_direction)
    return;

  g_object_freeze_notify (style_object);

  rtl = (direction == GTK_TEXT_DIR_RTL);

  if (style_priv->rtl != rtl) {
    style_priv->rtl = rtl;

    g_object_notify (style_object, AR_STYLE_PROP_RTL);
  }

  g_object_thaw_notify (style_object);
}

/* GtkWidgetClass impl */

static void
ar_clutter_embed_realize (GtkWidget *widget)
{
  ArClutterEmbed *embed = AR_CLUTTER_EMBED (widget);
  ArClutterEmbedPrivate *priv = embed->priv;
#ifndef HAVE_HILDON
  GdkDisplay *display;
#endif

  GTK_WIDGET_CLASS (ar_clutter_embed_parent_class)->realize (widget);

  /* FIXMEchpe: this isn't really HILDON, but don't-support-mouse */
#ifndef HAVE_HILDON
  /* Create cursors */
  display = gtk_widget_get_display (widget);

  priv->cursor[AR_CURSOR_DEFAULT] = gdk_cursor_new_for_display (display, GDK_LEFT_PTR);
  priv->cursor[AR_CURSOR_OPEN] = ar_cursor_new (widget->window, AR_CURSOR_OPEN);
  priv->cursor[AR_CURSOR_CLOSED] = ar_cursor_new (widget->window, AR_CURSOR_CLOSED);
  priv->cursor[AR_CURSOR_DROPPABLE] = gdk_cursor_new_for_display (display, GDK_DOUBLE_ARROW); /* FIXMEchpe: better cursor */
#endif /* !HAVE_HILDON */

  ar_clutter_embed_set_cursor (embed, AR_CURSOR_DEFAULT);
}

static void
ar_clutter_embed_unrealize (GtkWidget *widget)
{
  /* FIXMEchpe */
#ifndef HAVE_HILDON
  ArClutterEmbed *embed = AR_CLUTTER_EMBED (widget);
  ArClutterEmbedPrivate *priv = embed->priv;
  guint i;

  for (i = 0; i < AR_LAST_CURSOR; ++i) {
    gdk_cursor_unref (priv->cursor[i]);
    priv->cursor[i] = NULL;
  }
#endif /* !HAVE_HILDON*/

  GTK_WIDGET_CLASS (ar_clutter_embed_parent_class)->unrealize (widget);
}

static void
ar_clutter_embed_screen_changed (GtkWidget *widget,
                                  GdkScreen *previous_screen)
{
  ArClutterEmbed *embed = AR_CLUTTER_EMBED (widget);
  ArClutterEmbedPrivate *priv = embed->priv;
  GdkScreen *screen;
  GtkSettings *settings;
  void (* screen_changed) (GtkWidget*, GdkScreen *) =
    GTK_WIDGET_CLASS (ar_clutter_embed_parent_class)->screen_changed;

  if (screen_changed) {
    screen_changed (widget, previous_screen);
  }

  g_assert (priv->style != NULL);

  screen = gtk_widget_get_screen (widget);
  if (screen == previous_screen)
    return;

  if (previous_screen != NULL) {
    g_signal_handlers_disconnect_by_func (gtk_settings_get_for_screen (previous_screen),
                                          G_CALLBACK (sync_settings),
                                          embed);
  }

  if (screen == NULL)
    return;

  settings = gtk_settings_get_for_screen (screen);

  sync_settings (settings, NULL, embed);
  g_signal_connect (settings, "notify::gtk-double-click-time",
                    G_CALLBACK (sync_settings), embed);
  g_signal_connect (settings, "notify::gtk-enable-animations",
                    G_CALLBACK (sync_settings), embed);
#if GTK_CHECK_VERSION (2, 14, 0)
  g_signal_connect (settings, "notify::gtk-enable-event-sounds",
                    G_CALLBACK (sync_settings), embed);
#endif /* GTK+ >= 2.14.0 */
#if GTK_CHECK_VERSION (2, 10, 0)
  g_signal_connect (settings, "notify::gtk-touchscreen-mode",
                    G_CALLBACK (sync_settings), embed);
#endif /* GTK+ >= 2.10.0 */
}

static void
ar_clutter_embed_style_set (GtkWidget *widget,
                             GtkStyle *previous_style)
{
  ArClutterEmbed *embed = AR_CLUTTER_EMBED (widget);
  ArClutterEmbedPrivate *embed_priv = embed->priv;
  ArStyle *style = embed_priv->style;
  ArStylePrivate *style_priv = style->priv;
  GObject *style_object = G_OBJECT (style);
  GdkColor *color = NULL;
  ClutterColor selection_color;
  int focus_line_width, focus_padding;
  gboolean interior_focus;
  double card_slot_ratio;

  GTK_WIDGET_CLASS (ar_clutter_embed_parent_class)->style_set (widget, previous_style);

  g_object_freeze_notify (style_object);

  gtk_widget_style_get (widget,
                        "interior-focus", &interior_focus,
                        "focus-line-width", &focus_line_width,
                        "focus-padding", &focus_padding,
                        "card-slot-ratio", &card_slot_ratio,
                        "selection-color", &color,
                        NULL);

  if (style_priv->interior_focus != interior_focus) {
    style_priv->interior_focus = interior_focus;

    g_object_notify (style_object, AR_STYLE_PROP_INTERIOR_FOCUS);
  }

  if (style_priv->focus_line_width != focus_line_width) {
    style_priv->focus_line_width = focus_line_width;

    g_object_notify (style_object, AR_STYLE_PROP_FOCUS_LINE_WIDTH);
  }

  if (style_priv->focus_padding != focus_padding) {
    style_priv->focus_padding = focus_padding;

    g_object_notify (style_object, AR_STYLE_PROP_FOCUS_PADDING);
  }

  if (style_priv->card_slot_ratio != card_slot_ratio) {
    style_priv->card_slot_ratio = card_slot_ratio;

    g_object_notify (style_object, AR_STYLE_PROP_CARD_SLOT_RATIO);
  }

  if (color != NULL) {
    _ar_clutter_color_from_gdk_color (&selection_color, color);
    gdk_color_free (color);
  } else {
    _ar_clutter_color_from_gdk_color (&selection_color, &default_selection_color);
  }

  if (!clutter_color_equal (&style_priv->selection_color, &selection_color)) {
    style_priv->selection_color = selection_color;

    g_object_notify (style_object, AR_STYLE_PROP_SELECTION_COLOR);
  }

  g_object_thaw_notify (style_object);
}

static void
ar_clutter_embed_direction_changed (GtkWidget *widget,
                                    GtkTextDirection previous_direction)
{
  ArClutterEmbed *embed = AR_CLUTTER_EMBED (widget);

  GTK_WIDGET_CLASS (ar_clutter_embed_parent_class)->direction_changed (widget, previous_direction);

  sync_direction (embed, previous_direction);
}

static gboolean
ar_clutter_embed_focus_in (GtkWidget *widget,
                           GdkEventFocus *event)
{
  gboolean retval;

  retval = GTK_WIDGET_CLASS (ar_clutter_embed_parent_class)->focus_in_event (widget, event);

#if 0
  ClutterActor *stage;
  stage = gtk_clutter_embed_get_stage (GTK_CLUTTER_EMBED (widget));
  clutter_stage_set_key_focus (CLUTTER_STAGE (stage), FIXME board actor);
#endif

  return retval;
}

static gboolean
ar_clutter_embed_focus_out (GtkWidget *widget,
                            GdkEventFocus *event)
{
#ifdef FIXMEchpe
  clear_state (board);
#endif

  return GTK_WIDGET_CLASS (ar_clutter_embed_parent_class)->focus_out_event (widget, event);
}

static gboolean
ar_clutter_embed_focus (GtkWidget *widget,
                        GtkDirectionType direction)
{
//   ArClutterEmbed *embed = AR_CLUTTER_EMBED (embed);
//   ArClutterEmbedPrivate *priv = embed->priv;
  int count;
  gboolean retval = FALSE;

  switch (direction) {
    case GTK_DIR_TAB_FORWARD:
      count = 1;
      break;
    case GTK_DIR_TAB_BACKWARD:
      count = -1;
      break;
    default:
      break;
  }

#ifdef FIXMEchpe
  g_signal_emit_by_name (priv->board_actor, "focus", count, &retval);
#endif

  if (retval)
    return TRUE;

  return GTK_WIDGET_CLASS (ar_clutter_embed_parent_class)->focus (widget, direction);
}

/* GObjectClass impl */

static void
ar_clutter_embed_init (ArClutterEmbed *embed)
{
  GtkWidget *widget = GTK_WIDGET (embed);

  embed->priv = G_TYPE_INSTANCE_GET_PRIVATE (embed, AR_TYPE_CLUTTER_EMBED, ArClutterEmbedPrivate);

  GTK_WIDGET_SET_FLAGS (widget, GTK_CAN_FOCUS);
}

static void
ar_clutter_embed_dispose (GObject *object)
{
  ArClutterEmbed *embed = AR_CLUTTER_EMBED (object);
  GtkWidget *widget = GTK_WIDGET (embed);

  g_signal_handlers_disconnect_by_func (gtk_widget_get_settings (widget),
                                        G_CALLBACK (sync_settings),
                                        embed);

  G_OBJECT_CLASS (ar_clutter_embed_parent_class)->dispose (object);
}

static void
ar_clutter_embed_finalize (GObject *object)
{
  ArClutterEmbed *embed = AR_CLUTTER_EMBED (object);
  ArClutterEmbedPrivate *priv = embed->priv;

  g_assert (priv->style != NULL);
  g_object_unref (priv->style);

  G_OBJECT_CLASS (ar_clutter_embed_parent_class)->finalize (object);
}

static void
ar_clutter_embed_set_property (GObject      *object,
                                guint         property_id,
                                const GValue *value,
                                GParamSpec   *pspec)
{
  ArClutterEmbed *embed = AR_CLUTTER_EMBED (object);
  ArClutterEmbedPrivate *priv = embed->priv;

  switch (property_id) {
    case PROP_STYLE:
      priv->style = g_value_dup_object (value);

      /* This is necessary since we don't get an initial change notification! */
      sync_direction (embed, GTK_TEXT_DIR_LTR);

      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_clutter_embed_class_init (ArClutterEmbedClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  g_type_class_add_private (klass, sizeof (ArClutterEmbedPrivate));

  object_class->set_property = ar_clutter_embed_set_property;
  object_class->dispose = ar_clutter_embed_dispose;
  object_class->finalize = ar_clutter_embed_finalize;

  widget_class->realize = ar_clutter_embed_realize;
  widget_class->unrealize = ar_clutter_embed_unrealize;
  widget_class->style_set = ar_clutter_embed_style_set;
  widget_class->direction_changed = ar_clutter_embed_direction_changed;
  widget_class->screen_changed = ar_clutter_embed_screen_changed;
  widget_class->focus_in_event = ar_clutter_embed_focus_in;
  widget_class->focus_out_event = ar_clutter_embed_focus_out;
  widget_class->focus = ar_clutter_embed_focus;

  /**
   * ArClutterEmbed:style:
   *
   * An #ArStyle that @embed will update with its widget style properties.
   */
  g_object_class_install_property
    (object_class,
     PROP_STYLE,
     g_param_spec_object ("style", NULL, NULL,
                          AR_TYPE_STYLE,
                          G_PARAM_WRITABLE |
                          G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS));

  gtk_widget_class_install_style_property
    (widget_class,
     g_param_spec_boxed ("selection-color", NULL, NULL,
                         GDK_TYPE_COLOR,
                         G_PARAM_READWRITE |
                         G_PARAM_STATIC_STRINGS));

  /**
   * ArClutterEmbed:card-slot-ratio:
   *
   * The ratio of card to slot size. Note that this is the ratio of
   * card width/slot width and card height/slot height, not of
   * card area/slot area.
  */
  gtk_widget_class_install_style_property
    (widget_class,
     g_param_spec_double ("card-slot-ratio", NULL, NULL,
                          0.1, 1.0, DEFAULT_CARD_SLOT_RATIO,
                          G_PARAM_READWRITE |
                          G_PARAM_STATIC_STRINGS));
}

/* public API */

/**
 * ar_clutter_embed_new:
 * @style: an #ArStyle
 *
 * Returns: a new #ArClutterEmbed
 */
ArClutterEmbed *
ar_clutter_embed_new (ArStyle *style)
{
  return g_object_new (AR_TYPE_CLUTTER_EMBED,
                       "style", style,
                       NULL);
}

/**
 * ar_clutter_embed_set_cursor:
 * @embed: an #ArClutterEmbed
 * @cursor_type: the cursor type
 *
 * Sets the cursor on @embed to @cursor_type.
 */
void
ar_clutter_embed_set_cursor (ArClutterEmbed *embed,
                             ArCursorType cursor)
{
  /* FIXMEchpe */
#ifndef HAVE_HILDON
  ArClutterEmbedPrivate *priv = embed->priv;

  gdk_window_set_cursor (GTK_WIDGET (embed)->window,
                         priv->cursor[cursor]);
#endif /* !HAVE_HILDON */
}
