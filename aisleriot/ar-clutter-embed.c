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

#include "ar-clutter-embed.h"

#include "ar-style.h"
#include "ar-style-gtk.h"

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
  ArClutterEmbedPrivate *priv = embed->priv;

  if (priv->style != NULL) {
    _ar_style_gtk_detach (priv->style, GTK_WIDGET (embed));

    g_object_unref (priv->style);
    priv->style = NULL;
  }

  G_OBJECT_CLASS (ar_clutter_embed_parent_class)->dispose (object);
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
      _ar_style_gtk_attach (priv->style, GTK_WIDGET (embed));
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

  widget_class->realize = ar_clutter_embed_realize;
  widget_class->unrealize = ar_clutter_embed_unrealize;
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

  _ar_style_gtk_class_install_style_properties (widget_class);
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
