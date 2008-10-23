/* gtk-clutter-embed.c: Embeddable ClutterStage
 *
 * Copyright (C) 2007 OpenedHand
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not see <http://www.fsf.org/licensing>.
 *
 * Authors:
 *   Iain Holmes  <iain@openedhand.com>
 *   Emmanuele Bassi  <ebassi@openedhand.com>
 */

/**
 * SECTION:gtk-clutter-embed
 * @short_description: Widget for embedding a Clutter scene
 *
 * #AisleriotClutterEmbed is a GTK+ widget embedding a #ClutterStage. Using
 * a #AisleriotClutterEmbed widget is possible to build, show and interact with
 * a scene built using Clutter inside a GTK+ application.
 *
 * <note>To avoid flickering on show, you should call gtk_widget_show()
 * or gtk_widget_realize() before calling clutter_actor_show() on the
 * embedded #ClutterStage actor. This is needed for Clutter to be able
 * to paint on the #AisleriotClutterEmbed widget.</note>
 *
 * Since: 0.6
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib-object.h>

#include <gdk/gdk.h>
#include <gtk/gtkmain.h>

#include <clutter/clutter-main.h>
#include <clutter/clutter-stage.h>
#include <clutter/clutter-container.h>

#if defined(GDK_WINDOWING_X11)

#include <clutter/x11/clutter-x11.h>
#include <gdk/gdkx.h>

#elif defined(GDK_WINDOWING_WIN32)

#include <clutter/clutter-win32.h>
#include <gdk/gdkwin32.h>

#endif /* HAVE_CLUTTER_GTK_{X11,WIN32} */

#include "clutter-embed.h"

G_DEFINE_TYPE (AisleriotClutterEmbed, aisleriot_clutter_embed, GTK_TYPE_WIDGET);

#define AISLERIOT_CLUTTER_EMBED_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), AISLERIOT_TYPE_CLUTTER_EMBED, AisleriotClutterEmbedPrivate))

struct _AisleriotClutterEmbedPrivate
{
  ClutterActor *stage;
};

static void
aisleriot_clutter_embed_send_configure (AisleriotClutterEmbed *embed)
{
  GtkWidget *widget;
  GdkEvent *event = gdk_event_new (GDK_CONFIGURE);

  widget = GTK_WIDGET (embed);

  event->configure.window = g_object_ref (widget->window);
  event->configure.send_event = TRUE;
  event->configure.x = widget->allocation.x;
  event->configure.y = widget->allocation.y;
  event->configure.width = widget->allocation.width;
  event->configure.height = widget->allocation.height;
  
  gtk_widget_event (widget, event);
  gdk_event_free (event);
}

static void
aisleriot_clutter_embed_dispose (GObject *gobject)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (gobject)->priv;

  if (priv->stage)
    {
      clutter_actor_destroy (priv->stage);
      priv->stage = NULL;
    }

  G_OBJECT_CLASS (aisleriot_clutter_embed_parent_class)->dispose (gobject);
}

static void
aisleriot_clutter_embed_show (GtkWidget *widget)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;

  if (GTK_WIDGET_REALIZED (widget))
    clutter_actor_show (priv->stage);

  GTK_WIDGET_CLASS (aisleriot_clutter_embed_parent_class)->show (widget);
}

static void
aisleriot_clutter_embed_hide (GtkWidget *widget)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;

  clutter_actor_hide (priv->stage);

  GTK_WIDGET_CLASS (aisleriot_clutter_embed_parent_class)->hide (widget);
}

static void
aisleriot_clutter_embed_realize (GtkWidget *widget)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv; 
  GdkWindowAttr attributes;
  int attributes_mask;
  
  /* we must realize the stage to get it ready for embedding */
  clutter_actor_realize (priv->stage);

#ifdef GDK_WINDOWING_X11
  {
    const XVisualInfo *xvinfo;
    GdkVisual *visual;
    GdkColormap *colormap;

    /* We need to use the colormap from the Clutter visual */
    xvinfo = clutter_x11_get_stage_visual (CLUTTER_STAGE (priv->stage));
    visual = gdk_x11_screen_lookup_visual (gtk_widget_get_screen (widget),
                                           xvinfo->visualid);
    colormap = gdk_colormap_new (visual, FALSE);
    gtk_widget_set_colormap (widget, colormap);
  }
#endif

  GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);

  /* NOTE: GDK_MOTION_NOTIFY above should be safe as Clutter does its own
   *       throtling. 
  */
  attributes.event_mask = gtk_widget_get_events (widget)
                        | GDK_EXPOSURE_MASK
                        | GDK_BUTTON_PRESS_MASK
                        | GDK_BUTTON_RELEASE_MASK
                        | GDK_KEY_PRESS_MASK
                        | GDK_KEY_RELEASE_MASK
                        | GDK_POINTER_MOTION_MASK;

  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

  widget->window = gdk_window_new (gtk_widget_get_parent_window (widget),
                                   &attributes,
                                   attributes_mask);
  gdk_window_set_user_data (widget->window, widget);

  widget->style = gtk_style_attach (widget->style, widget->window);
  gtk_style_set_background (widget->style, widget->window, GTK_STATE_NORMAL);
  
  gdk_window_set_back_pixmap (widget->window, NULL, FALSE);

#if defined(GDK_WINDOWING_X11)
  clutter_x11_set_stage_foreign (CLUTTER_STAGE (priv->stage), 
                                 GDK_WINDOW_XID (widget->window));
#elif defined(GDK_WINDOWING_WIN32)
  clutter_win32_set_stage_foreign (CLUTTER_STAGE (priv->stage), 
				   GDK_WINDOW_HWND (widget->window));
#endif /* GDK_WINDOWING_{X11,WIN32} */

  clutter_actor_queue_redraw (CLUTTER_ACTOR (priv->stage));

  if (GTK_WIDGET_VISIBLE (widget))
    clutter_actor_show (priv->stage);

  aisleriot_clutter_embed_send_configure (AISLERIOT_CLUTTER_EMBED (widget));
}

static void
aisleriot_clutter_embed_size_allocate (GtkWidget     *widget,
                                 GtkAllocation *allocation)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;

  widget->allocation = *allocation;

  if (GTK_WIDGET_REALIZED (widget))
    {
      gdk_window_move_resize (widget->window,
                              allocation->x, allocation->y,
                              allocation->width, allocation->height);

      aisleriot_clutter_embed_send_configure (AISLERIOT_CLUTTER_EMBED (widget));
    }

  clutter_actor_set_size (priv->stage,
                          allocation->width,
                          allocation->height);

  clutter_actor_queue_relayout (priv->stage);
}

static gboolean
aisleriot_clutter_embed_motion_notify_event (GtkWidget      *widget,
                                       GdkEventMotion *event)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;
  ClutterEvent cevent = { 0, };

  cevent.type = CLUTTER_MOTION;
  cevent.any.stage = CLUTTER_STAGE (priv->stage);
  cevent.motion.x = event->x;
  cevent.motion.y = event->y;
  cevent.motion.time = event->time;

  clutter_do_event (&cevent);

  /* doh - motion events can push ENTER/LEAVE events onto Clutters
   * internal event queue which we do really ever touch (essentially
   * proxying from gtks queue). The below pumps them back out and
   * processes.
   * *could* be side effects with below though doubful as no other
   * events reach the queue (we shut down event collection). Maybe
   * a peek_mask type call could be even safer. 
  */
  while (clutter_events_pending())
    {
      ClutterEvent *ev = clutter_event_get ();
      if (ev)
        {
          clutter_do_event (ev);
          clutter_event_free (ev);
        }
    }

  return FALSE;
}

static gboolean
aisleriot_clutter_embed_button_event (GtkWidget      *widget,
                                GdkEventButton *event)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;
  ClutterEvent cevent = { 0, };

  if (event->type == GDK_BUTTON_PRESS ||
      event->type == GDK_2BUTTON_PRESS ||
      event->type == GDK_3BUTTON_PRESS)
    cevent.type = cevent.button.type = CLUTTER_BUTTON_PRESS;
  else if (event->type == GDK_BUTTON_RELEASE)
    cevent.type = cevent.button.type = CLUTTER_BUTTON_RELEASE;
  else
    return FALSE;

  cevent.any.stage = CLUTTER_STAGE (priv->stage);
  cevent.button.x = event->x;
  cevent.button.y = event->y;
  cevent.button.time = event->time;
  cevent.button.click_count =
    (event->type == GDK_BUTTON_PRESS ? 1
                                     : (event->type == GDK_2BUTTON_PRESS ? 2
                                                                         : 3));
  cevent.button.modifier_state = event->state;
  cevent.button.button = event->button;

  clutter_do_event (&cevent);

  return FALSE;
}

static gboolean
aisleriot_clutter_embed_key_event (GtkWidget   *widget,
                             GdkEventKey *event)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;
  ClutterEvent cevent = { 0, };

  if (event->type == GDK_KEY_PRESS)
    cevent.type = cevent.key.type = CLUTTER_KEY_PRESS;
  else if (event->type == GDK_KEY_RELEASE)
    cevent.type = cevent.key.type = CLUTTER_KEY_RELEASE;
  else
    return FALSE;

  cevent.any.stage = CLUTTER_STAGE (priv->stage);
  cevent.key.time = event->time;
  cevent.key.modifier_state = event->state;
  cevent.key.keyval = event->keyval;
  cevent.key.hardware_keycode = event->hardware_keycode;

  clutter_do_event (&cevent);

  return FALSE;
}

static gboolean
aisleriot_clutter_embed_expose_event (GtkWidget      *widget,
                                GdkEventExpose *event)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;

  if (CLUTTER_ACTOR_IS_VISIBLE (priv->stage))
    clutter_actor_queue_redraw (priv->stage);

  return FALSE;
}

static gboolean
aisleriot_clutter_embed_map_event (GtkWidget	 *widget,
                             GdkEventAny *event)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;

  /* The backend wont get the XEvent as we go strait to do_event().
   * So we have to make sure we set the event here.
  */
  CLUTTER_ACTOR_SET_FLAGS (priv->stage, CLUTTER_ACTOR_MAPPED);

  return FALSE;
}

static gboolean
aisleriot_clutter_embed_focus_out (GtkWidget     *widget,
                             GdkEventFocus *event)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;

  /* give back key focus to the stage */
  clutter_stage_set_key_focus (CLUTTER_STAGE (priv->stage), NULL);

  return FALSE;
}

static gboolean
aisleriot_clutter_embed_scroll_event (GtkWidget      *widget,
                                GdkEventScroll *event)
{
  AisleriotClutterEmbedPrivate *priv = AISLERIOT_CLUTTER_EMBED (widget)->priv;

  ClutterEvent cevent = { 0, };

  if (event->type == GDK_SCROLL)
    cevent.type = cevent.scroll.type = CLUTTER_SCROLL;
  else
    return FALSE;

  cevent.any.stage = CLUTTER_STAGE (priv->stage);
  cevent.scroll.x = (gint) event->x;
  cevent.scroll.y = (gint) event->y;
  cevent.scroll.time = event->time;
  cevent.scroll.direction = event->direction;
  cevent.scroll.modifier_state = event->state;

  clutter_do_event (&cevent);

  return FALSE;
}

static void
aisleriot_clutter_embed_class_init (AisleriotClutterEmbedClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  g_type_class_add_private (klass, sizeof (AisleriotClutterEmbedPrivate));

  gobject_class->dispose = aisleriot_clutter_embed_dispose;

  widget_class->size_allocate = aisleriot_clutter_embed_size_allocate;
  widget_class->realize = aisleriot_clutter_embed_realize;
  widget_class->show = aisleriot_clutter_embed_show;
  widget_class->hide = aisleriot_clutter_embed_hide;
  widget_class->button_press_event = aisleriot_clutter_embed_button_event;
  widget_class->button_release_event = aisleriot_clutter_embed_button_event;
  widget_class->key_press_event = aisleriot_clutter_embed_key_event;
  widget_class->key_release_event = aisleriot_clutter_embed_key_event;
  widget_class->motion_notify_event = aisleriot_clutter_embed_motion_notify_event;
  widget_class->expose_event = aisleriot_clutter_embed_expose_event;
  widget_class->map_event = aisleriot_clutter_embed_map_event;
  widget_class->focus_out_event = aisleriot_clutter_embed_focus_out;
  widget_class->scroll_event = aisleriot_clutter_embed_scroll_event;
}

static void
aisleriot_clutter_embed_init (AisleriotClutterEmbed *embed)
{
  AisleriotClutterEmbedPrivate *priv;

  embed->priv = priv = AISLERIOT_CLUTTER_EMBED_GET_PRIVATE (embed);

  GTK_WIDGET_SET_FLAGS (embed, GTK_CAN_FOCUS);

  /* disable double-buffering: it's automatically provided
   * by OpenGL
   */
  gtk_widget_set_double_buffered (GTK_WIDGET (embed), FALSE);

  /* we always create new stages rather than use the default */
  priv->stage = clutter_stage_new ();
}

/**
 * aisleriot_clutter_init:
 * @argc: pointer to the arguments count, or %NULL
 * @argv: pointer to the arguments vector, or %NULL
 *
 * This function should be called instead of clutter_init() and
 * gtk_init().
 *
 * Return value: %CLUTTER_INIT_SUCCESS on success, a negative integer
 *   on failure.
 *
 * Since: 0.8
 */
ClutterInitError
aisleriot_clutter_init (int    *argc,
                  char ***argv)
{
  if (!gtk_init_check (argc, argv))
    return AISLERIOT_CLUTTER_INIT_ERROR_GTK;

#if defined(GDK_WINDOWING_X11)
  clutter_x11_set_display (GDK_DISPLAY());
  clutter_x11_disable_event_retrieval ();
#elif defined(GDK_WINDOWING_WIN32)
  clutter_win32_disable_event_retrieval ();
#endif /* HAVE_CLUTTER_GTK_{X11,WIN32} */

  return clutter_init (argc, argv);
}

/**
 * aisleriot_clutter_embed_new:
 *
 * Creates a new #AisleriotClutterEmbed widget. This widget can be
 * used to build a scene using Clutter API into a GTK+ application.
 *
 * Return value: the newly created #AisleriotClutterEmbed
 *
 * Since: 0.6
 */
GtkWidget *
aisleriot_clutter_embed_new (void)
{
  return g_object_new (AISLERIOT_TYPE_CLUTTER_EMBED, NULL);
}

/**
 * aisleriot_clutter_embed_get_stage:
 * @embed: a #AisleriotClutterEmbed
 *
 * Retrieves the #ClutterStage from @embed. The returned stage can be
 * used to add actors to the Clutter scene.
 *
 * Return value: the Clutter stage. You should never destroy or unref
 *   the returned actor.
 *
 * Since: 0.6
 */
ClutterActor *
aisleriot_clutter_embed_get_stage (AisleriotClutterEmbed *embed)
{
  g_return_val_if_fail (AISLERIOT_IS_CLUTTER_EMBED (embed), NULL);

  return embed->priv->stage;
}
