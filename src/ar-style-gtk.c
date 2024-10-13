/*
 * Copyright © 1998, 2003 Jonathan Blandford <jrb@mit.edu>
 * Copyright © 2007, 2008, 2009, 2010 Christian Persch
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

#include "ar-style-gtk.h"

#include "ar-style-private.h"

#include "ar-debug.h"

/**
 * SECTION: ar-style-gtk
 * @short_description: common functions to set #ArStyle properties
 *
 * Tie #ArStyle properties to #GtkWidget style properties, and to
 * #GtkSettings properties.
 */

#define I_(string) (g_intern_static_string (string))

/* private functions */

/*
 * ar_style_provider_new:
 * 
 * Returns: (transfer full): a new #GtkStyleProvider
 */
static GtkStyleProvider *
ar_style_provider_new (void)
{
  static const char css[] =
    " AisleriotBoard, aisleriot-board {\n"
      "background-color: " DEFAULT_BACKGROUND_COLOR_STRING ";\n"
      "-AisleriotBoard-selection-color: " DEFAULT_SELECTION_COLOR_STRING ";\n"
      "background-image: url('resource:///org/gnome/aisleriot/art/baize.png');\n"
      "background-repeat: repeat;\n"
  "}\n";
#undef NAME

  GtkCssProvider *provider;
  GError *err = NULL;

  provider = gtk_css_provider_new ();
  gtk_css_provider_load_from_data (provider, css, -1,  &err);
  if (err)
    g_error ("ERROR: %s\n", err->message);

  return GTK_STYLE_PROVIDER (provider);
}

static void
sync_settings (GtkSettings *settings,
               GParamSpec *pspec,
               ArStyle *style)
{
  ArStylePrivate *style_priv = ar_style_get_instance_private_exported (style);
  GObject *style_object = G_OBJECT (style);
  const char *pspec_name;

  if (pspec)
    pspec_name = pspec->name;
  else
    pspec_name = NULL;

  ar_debug_print (AR_DEBUG_GAME_STYLE,
                      "[ArStyle %p] Syncing GtkSettings:%s\n",
                      style, pspec_name ? pspec_name : "*");

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

  if (pspec_name == NULL || pspec_name == I_("gtk-enable-event-sounds")) {
    gboolean enable;

    g_object_get (settings, "gtk-enable-event-sounds", &enable, NULL);

    if (enable != style_priv->enable_sound_gtk) {
      style_priv->enable_sound_gtk = enable;

      /* FIXMEchpe: only notify if the effective setting changed */
      g_object_notify (style_object, AR_STYLE_PROP_ENABLE_SOUND);
    }
  }

  if (pspec_name == NULL || pspec_name == I_("gtk-touchscreen-mode")) {
    gboolean enable;

    g_object_get (settings, "gtk-touchscreen-mode", &enable, NULL);

    if (enable != style_priv->touchscreen_mode) {
      style_priv->touchscreen_mode = enable;

      /* FIXMEchpe: only notify if the effective setting changed */
      g_object_notify (style_object, AR_STYLE_PROP_TOUCHSCREEN_MODE);
    }
  }

  g_object_thaw_notify (style_object);
}

static void
direction_changed_cb (GtkWidget *widget,
                      GtkTextDirection previous_direction,
                      ArStyle *style)
{
  ArStylePrivate *style_priv = ar_style_get_instance_private_exported (style);
  GObject *style_object = G_OBJECT (style);
  GtkTextDirection direction;
  gboolean rtl;

  direction = gtk_widget_get_direction (widget);

  ar_debug_print (AR_DEBUG_GAME_STYLE,
                      "[ArStyle %p] Widget direction-changed direction %d previous-direction %d\n",
                      style, direction, previous_direction);

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

static void
screen_changed_cb (GtkWidget *widget,
                   GdkScreen *previous_screen,
                   ArStyle *style)
{
  GdkScreen *screen;
  GtkSettings *settings;

  g_assert (style != NULL);

  screen = gtk_widget_get_screen (widget);

  ar_debug_print (AR_DEBUG_GAME_STYLE,
                      "[ArStyle %p] Widget screen-changed screen %p previous-screen %p\n",
                      style, screen, previous_screen);

  if (screen == previous_screen)
    return;

  if (previous_screen != NULL) {
    g_signal_handlers_disconnect_by_func (gtk_settings_get_for_screen (previous_screen),
                                          G_CALLBACK (sync_settings),
                                          style);
  }

  if (screen == NULL)
    return;

  settings = gtk_settings_get_for_screen (screen);

  sync_settings (settings, NULL, style);

  g_signal_connect (settings, "notify::gtk-double-click-time",
                    G_CALLBACK (sync_settings), style);
  g_signal_connect (settings, "notify::gtk-enable-animations",
                    G_CALLBACK (sync_settings), style);
  g_signal_connect (settings, "notify::gtk-enable-event-sounds",
                    G_CALLBACK (sync_settings), style);
  g_signal_connect (settings, "notify::gtk-touchscreen-mode",
                    G_CALLBACK (sync_settings), style);
}

static void
style_updated_cb (GtkWidget *widget,
                  ArStyle *style)
{
  ArStylePrivate *style_priv = ar_style_get_instance_private_exported (style);
  GObject *style_object = G_OBJECT (style);
  GdkRGBA *selection_color;
  int focus_line_width, focus_padding;
  gboolean interior_focus;
  double card_slot_ratio, card_overhang, card_step;

  ar_debug_print (AR_DEBUG_GAME_STYLE,
                      "[ArStyle %p] Syncing widget style properties\n",
                      style);

  g_object_freeze_notify (style_object);

  gtk_widget_style_get (widget,
                        "interior-focus", &interior_focus,
                        "focus-line-width", &focus_line_width,
                        "focus-padding", &focus_padding,
                        "card-slot-ratio", &card_slot_ratio,
                        "card-overhang", &card_overhang,
                        "card-step", &card_step,
                        "selection-color", &selection_color,
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

  if (style_priv->card_overhang != card_overhang) {
    style_priv->card_overhang = card_overhang;

    g_object_notify (style_object, AR_STYLE_PROP_CARD_OVERHANG);
  }

  if (style_priv->card_step != card_step) {
    style_priv->card_step = card_step;

    g_object_notify (style_object, AR_STYLE_PROP_CARD_STEP);
  }

  if (!gdk_rgba_equal (&style_priv->selection_color, selection_color)) {
    style_priv->selection_color = *selection_color;

    g_object_notify (style_object, AR_STYLE_PROP_SELECTION_COLOR);
  }
  gdk_rgba_free (selection_color);

  g_object_thaw_notify (style_object);
}

/* private API */

/**
 * _ar_style_gtk_class_install_style_properties:
 * @widget_class: a #GtkWidgetClass
 *
 * Installs style properties in @widget_class.
 */
void
_ar_style_gtk_class_install_style_properties (GtkWidgetClass *widget_class)
{
  /**
   * ArClutterEmbed:selection-color:
   *
   * The card selection colour.
  */
  gtk_widget_class_install_style_property
    (widget_class,
     g_param_spec_boxed ("selection-color", NULL, NULL,
                         GDK_TYPE_RGBA,
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

  /**
   * ArClutterEmbed:card-overhang:
   *
   * This controls how much of a card is allowed to hang off of the bottom
   * of the screen. If set to %0.0, the last card is always fully visible.
   */
  gtk_widget_class_install_style_property
    (widget_class,
     g_param_spec_double ("card-overhang", NULL, NULL,
                          0.0, 1.0, DEFAULT_CARD_OVERHANG,
                          G_PARAM_READWRITE |
                          G_PARAM_STATIC_STRINGS));

  /**
   * ArClutterEmbed:card-step:
   *
   * This controls how much one card is offset the previous one in card stacks.
   * A game-specified a value for the card step takes precedence over this.
   */
  gtk_widget_class_install_style_property
    (widget_class,
     g_param_spec_double ("card-step", NULL, NULL,
                          MIN_CARD_STEP, MAX_CARD_STEP, DEFAULT_CARD_STEP,
                          G_PARAM_READWRITE |
                          G_PARAM_STATIC_STRINGS));
}

/**
 * _ar_style_gtk_attach:
 * @style: a #ArStyle
 * @widget: a #GtkWidget
 *
 * Attaches @style to @widget's style properties and the properties
 * of its #GdkScreen's #GtkSettings object.
 */
void
_ar_style_gtk_attach (ArStyle *style,
                      GtkWidget *widget)
{
  GtkStyleContext *context;
  GtkStyleProvider *provider;

  g_return_if_fail (AR_IS_STYLE (style));
  g_return_if_fail (GTK_IS_WIDGET (widget));

  ar_debug_print (AR_DEBUG_GAME_STYLE,
                      "[ArStyle %p] Attaching to widget %p\n", style, widget);

  context = gtk_widget_get_style_context (widget);
  provider = ar_style_provider_new ();
  gtk_style_context_add_provider (context, provider,
                                  GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref (provider);

  g_assert (g_object_get_data (G_OBJECT (widget), "Ar::Style") == NULL);
  g_object_set_data (G_OBJECT (widget), "Ar::Style", style);

  /* This is necessary since we don't get an initial change notification! */
  direction_changed_cb (widget, GTK_TEXT_DIR_LTR, style);
  style_updated_cb (widget, style);

  g_signal_connect (widget, "style-updated",
                    G_CALLBACK (style_updated_cb), style);
  g_signal_connect (widget, "screen-changed",
                    G_CALLBACK (screen_changed_cb), style);
  g_signal_connect (widget, "direction-changed",
                    G_CALLBACK (direction_changed_cb), style);
}

/**
 * _ar_style_gtk_detach:
 * @style: a #ArStyle
 * @widget: a #GtkWidget
 *
 * Detaches @style from @widget.
 */
void
_ar_style_gtk_detach (ArStyle *style,
                      GtkWidget *widget)
{
  g_return_if_fail (AR_IS_STYLE (style));
  g_return_if_fail (GTK_IS_WIDGET (widget));

  ar_debug_print (AR_DEBUG_GAME_STYLE,
                      "[ArStyle %p] Detaching from widget %p\n", style, widget);

  g_assert (g_object_get_data (G_OBJECT (widget), "Ar::Style") == style);
  g_object_set_data (G_OBJECT (widget), "Ar::Style", NULL);

  g_signal_handlers_disconnect_by_func (widget, G_CALLBACK (style_updated_cb), style);
  g_signal_handlers_disconnect_by_func (widget, G_CALLBACK (screen_changed_cb), style);
  g_signal_handlers_disconnect_by_func (widget, G_CALLBACK (direction_changed_cb), style);

  g_signal_handlers_disconnect_by_func (gtk_widget_get_settings (widget),
                                        G_CALLBACK (sync_settings),
                                        style);
}
