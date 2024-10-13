/*  
 * Copyright © 2009 Christian Persch <chpe@src.gnome.org>
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

#include "ar-fullscreen-button.h"
#include "ar-stock.h"


#define AUTOHIDE_TIMEOUT (5 /* s */)


enum {
  PROP_0,
  PROP_ACTIVE,
  PROP_CORNER,
  PROP_WINDOW
};


struct _ArFullscreenButton {
  GtkWindow parent_instance;
  GtkWindow *window;

  GtkCornerType corner;
  GtkCornerType effective_corner;

  guint autohide_timeout_id;
  guint active : 1;
};

G_DEFINE_TYPE (ArFullscreenButton, ar_fullscreen_button, GTK_TYPE_WINDOW);

/* private functions */

static gboolean
autohide_cb (ArFullscreenButton *self)
{
  self->autohide_timeout_id = 0;

  gtk_widget_hide (GTK_WIDGET (self));

  return FALSE;
}

static void
autohide_cancel (ArFullscreenButton *self)
{
  g_clear_handle_id (&self->autohide_timeout_id, g_source_remove);
}

static void
autohide_reschedule (ArFullscreenButton *button)
{
  ArFullscreenButtonPrivate *priv = button->priv;

  autohide_cancel (button);
  if (!priv->active)
    return;

  priv->autohide_timeout_id =
    gdk_threads_add_timeout_seconds (AUTOHIDE_TIMEOUT,
                                     (GSourceFunc) autohide_cb,
                                     button);
}

static void
update_position (ArFullscreenButton *button)
{
  ArFullscreenButtonPrivate *priv = button->priv;
  GtkWidget *widget = GTK_WIDGET (button);
  GtkRequisition requisition;
  GdkScreen *screen;
  GdkRectangle screen_rect;

  if (!gtk_widget_has_screen (widget) ||
      gtk_widget_get_style (widget) == NULL)
    return;

  gtk_widget_size_request (widget, &requisition);

  screen = gtk_widget_get_screen (widget);
  gdk_screen_get_monitor_workarea
    (screen,
     gtk_widget_get_realized (widget)
        ? gdk_screen_get_monitor_at_window (screen, gtk_widget_get_window (widget))
        : 0,
     &screen_rect);

  switch (priv->effective_corner) {
    case GTK_CORNER_TOP_LEFT:
    gtk_window_move (GTK_WINDOW (widget),
                     screen_rect.x, screen_rect.y);
      break;
    case GTK_CORNER_TOP_RIGHT:
      gtk_window_move (GTK_WINDOW (widget),
                       screen_rect.x + screen_rect.width - requisition.width,
                       screen_rect.y);
      break;
    case GTK_CORNER_BOTTOM_LEFT:
      gtk_window_move (GTK_WINDOW (widget),
                       screen_rect.x,
                       screen_rect.y + screen_rect.height - requisition.height);
      break;
    case GTK_CORNER_BOTTOM_RIGHT:
      gtk_window_move (GTK_WINDOW (widget),
                       screen_rect.x + screen_rect.width - requisition.width,
                       screen_rect.y + screen_rect.height - requisition.height);
      break;
  }
}

static void
update_screen (ArFullscreenButton *button,
               GdkScreen *previous_screen,
               GdkScreen *screen)
{
  if (screen == previous_screen)
    return;

  if (previous_screen != NULL) {
    g_signal_handlers_disconnect_matched (previous_screen, G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL, NULL, button);
  }

  if (screen == NULL)
    return;
        
  g_signal_connect_swapped (screen, "size-changed",
                            G_CALLBACK (update_position), button);

  /* FIXME: connect to composited changed? */
}

static void
set_active (ArFullscreenButton *button,
            gboolean active)
{
  ArFullscreenButtonPrivate *priv = button->priv;

  priv->active = active;

  if (active) {
    autohide_reschedule (button);
  } else {
    autohide_cancel (button);
  }

  g_object_set (button, "visible", active, NULL);
}

static void
set_corner (ArFullscreenButton *button,
            GtkCornerType corner)
{
  ArFullscreenButtonPrivate *priv = button->priv;

  priv->corner = corner;

  if (gtk_widget_get_direction (GTK_WIDGET (button)) == GTK_TEXT_DIR_RTL) {
    static const GtkCornerType swap_corners[] = {
      GTK_CORNER_TOP_RIGHT,
      GTK_CORNER_BOTTOM_RIGHT,
      GTK_CORNER_TOP_LEFT,
      GTK_CORNER_BOTTOM_LEFT
    };

    priv->effective_corner = swap_corners[corner];
  } else {
    priv->effective_corner = corner;
  }

  update_position (button);
}

static gboolean
window_button_press_cb (GtkWidget *window,
                        GdkEventButton *event,
                        ArFullscreenButton *button)
{
  if (event->type == GDK_BUTTON_PRESS) {
    ArFullscreenButtonPrivate *priv = button->priv;

    if (priv->active) {
      gtk_widget_show (GTK_WIDGET (button));
      autohide_reschedule (button);
    }
  }

  return FALSE;
}
    
static void
window_screen_changed_cb (GtkWidget *window,
                          GdkScreen *previous_screen,
                          ArFullscreenButton *button)
{
  GdkScreen *screen;

  screen = gtk_widget_get_screen (window);
  if (screen == gtk_widget_get_screen (GTK_WIDGET (button)))
    return;

  gtk_window_set_screen (GTK_WINDOW (button), screen);
}
    
/* GType impl */

G_DEFINE_TYPE (ArFullscreenButton, ar_fullscreen_button, GTK_TYPE_WINDOW)

/* GtkWidgetClass impl */

static void
ar_fullscreen_button_size_allocate (GtkWidget *widget,
                                    GtkAllocation *allocation)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (widget);

  GTK_WIDGET_CLASS (ar_fullscreen_button_parent_class)->size_allocate (widget, allocation);

  update_position (button);
}

static void
ar_fullscreen_button_realize (GtkWidget *widget)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (widget);
#ifdef GDK_WINDOWING_X11
  GdkScreen *screen;
  GdkColormap *colormap;

  screen = gtk_widget_get_screen (widget);
  if (gdk_screen_is_composited (screen) &&
      (colormap = gdk_screen_get_rgba_colormap (screen)) != NULL) {
    gtk_widget_set_colormap (widget, colormap);
  } else {
    gtk_widget_set_colormap (widget, gdk_screen_get_default_colormap (screen));
  }
#endif

  GTK_WIDGET_CLASS (ar_fullscreen_button_parent_class)->realize (widget);

  update_position (button);
}

static void
ar_fullscreen_button_show (GtkWidget *widget)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (widget);

  update_position (button);
  autohide_reschedule (button);

  GTK_WIDGET_CLASS (ar_fullscreen_button_parent_class)->show (widget);
}

static void
ar_fullscreen_button_hide (GtkWidget *widget)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (widget);

  autohide_cancel (button);

  GTK_WIDGET_CLASS (ar_fullscreen_button_parent_class)->hide (widget);
}

static gboolean
ar_fullscreen_button_expose (GtkWidget *widget,
                             GdkEventExpose *event)
{
  return GTK_WIDGET_CLASS (ar_fullscreen_button_parent_class)->expose_event (widget, event);
}

static gboolean
ar_fullscreen_button_button_release (GtkWidget *widget,
                                     GdkEventButton *event)
{
  if (event->button == 1) {
    ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (widget);
    ArFullscreenButtonPrivate *priv = button->priv;

    gtk_window_unfullscreen (priv->window);
    return TRUE;
  }

  return GTK_WIDGET_CLASS (ar_fullscreen_button_parent_class)->button_press_event (widget, event);
}

static void
ar_fullscreen_button_style_set (GtkWidget *widget,
                                GtkStyle *previous_style)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (widget);
  void (* style_set) (GtkWidget*, GtkStyle*) =
    GTK_WIDGET_CLASS (ar_fullscreen_button_parent_class)->style_set;

  if (style_set) {
    style_set (widget, previous_style);
  }

  update_position (button);
}

static void
ar_fullscreen_button_direction_changed (GtkWidget *widget,
                                        GtkTextDirection previous_direction)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (widget);
  ArFullscreenButtonPrivate *priv = button->priv;

  GTK_WIDGET_CLASS (ar_fullscreen_button_parent_class)->direction_changed (widget, previous_direction);

  if (gtk_widget_get_direction (widget) != previous_direction) {
    /* Update the effective corner */
    set_corner (button, priv->corner);
  }
}

static void
ar_fullscreen_button_screen_changed (GtkWidget *widget,
                                     GdkScreen *previous_screen)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (widget);
  void (* screen_changed) (GtkWidget*, GdkScreen*) =
    GTK_WIDGET_CLASS (ar_fullscreen_button_parent_class)->screen_changed;

  if (screen_changed) {
    screen_changed (widget, previous_screen);
  }

  update_screen (button, previous_screen, gtk_widget_get_screen (widget));
}

/* GObjectClass impl */

static void
ar_fullscreen_button_init (ArFullscreenButton *button)
{
  ArFullscreenButtonPrivate *priv;
  GtkWidget *widget = GTK_WIDGET (button);
  GtkWindow *window = GTK_WINDOW (button);
  GtkWidget *ebox, *image;

  priv = button->priv = G_TYPE_INSTANCE_GET_PRIVATE (button, AR_TYPE_FULLSCREEN_BUTTON, ArFullscreenButtonPrivate);

  priv->active = FALSE;
  priv->corner = priv->effective_corner = GTK_CORNER_BOTTOM_RIGHT;
  priv->window = NULL;
        
  gtk_window_set_resizable (window, FALSE);
  gtk_window_set_type_hint (window, GDK_WINDOW_TYPE_HINT_NOTIFICATION);
  gtk_window_set_skip_taskbar_hint (window, TRUE);
  gtk_window_set_skip_pager_hint (window, TRUE);
  gtk_window_set_focus_on_map (window, FALSE);

  /* Now create the contents */
  ebox = gtk_event_box_new ();
  gtk_event_box_set_visible_window (GTK_EVENT_BOX (ebox), FALSE);
  gtk_container_add (GTK_CONTAINER (button), ebox);

  image = gtk_image_new_from_stock (AR_STOCK_LEAVE_FULLSCREEN, GTK_ICON_SIZE_LARGE_TOOLBAR);
  gtk_container_add (GTK_CONTAINER (ebox), image);
  gtk_widget_show_all (ebox);

  /* Need to do this explicitly since there's no initial notification */
  update_screen (button, NULL, gtk_widget_get_screen (widget));
}

static void
ar_fullscreen_button_dispose (GObject *object)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (object);
  ArFullscreenButtonPrivate *priv = button->priv;

  update_screen (button, gtk_widget_get_screen (GTK_WIDGET (button)), NULL);

  if (priv->window != NULL) {
    g_signal_handlers_disconnect_matched (priv->window,
                                          G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL, button);

    priv->window = NULL;
  }

  autohide_cancel (button);

  G_OBJECT_CLASS (ar_fullscreen_button_parent_class)->dispose (object);
}

static void
ar_fullscreen_button_finalize (GObject *object)
{
  G_OBJECT_CLASS (ar_fullscreen_button_parent_class)->finalize (object);
}

static void
ar_fullscreen_button_get_property (GObject    *object,
                                   guint       property_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (object);
  ArFullscreenButtonPrivate *priv = button->priv;

  switch (property_id) {
    case PROP_ACTIVE:
      g_value_set_boolean (value, priv->active);
      break;

    case PROP_CORNER:
      g_value_set_enum (value, priv->corner);
      break;

    /* not readable */
    case PROP_WINDOW:
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_fullscreen_button_set_property (GObject      *object,
                                   guint         property_id,
                                   const GValue *value,
                                   GParamSpec   *pspec)
{
  ArFullscreenButton *button = AR_FULLSCREEN_BUTTON (object);
  ArFullscreenButtonPrivate *priv = button->priv;

  switch (property_id) {
    case PROP_ACTIVE:
      set_active (button, g_value_get_boolean (value));
      break;
    case PROP_CORNER:
      set_corner (button, g_value_get_enum (value));
      break;
    case PROP_WINDOW:
      priv->window = g_value_get_object (value);

      g_signal_connect (priv->window, "button-press-event",
                        G_CALLBACK (window_button_press_cb), button);
      g_signal_connect (priv->window, "screen-changed",
                        G_CALLBACK (window_screen_changed_cb), button);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_fullscreen_button_class_init (ArFullscreenButtonClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  g_type_class_add_private (klass, sizeof (ArFullscreenButtonPrivate));

  object_class->set_property = ar_fullscreen_button_set_property;
  object_class->get_property = ar_fullscreen_button_get_property;
  object_class->dispose = ar_fullscreen_button_dispose;
  object_class->finalize = ar_fullscreen_button_finalize;
  
  widget_class->realize = ar_fullscreen_button_realize;
  widget_class->screen_changed = ar_fullscreen_button_screen_changed;
  widget_class->style_set = ar_fullscreen_button_style_set;
  widget_class->direction_changed = ar_fullscreen_button_direction_changed;
  widget_class->size_allocate = ar_fullscreen_button_size_allocate;
  widget_class->show = ar_fullscreen_button_show;
  widget_class->hide = ar_fullscreen_button_hide;
  widget_class->expose_event = ar_fullscreen_button_expose;
  widget_class->button_release_event = ar_fullscreen_button_button_release;

  g_object_class_install_property
    (object_class,
     PROP_ACTIVE,
     g_param_spec_boolean ("active", NULL, NULL,
                           FALSE,
                           G_PARAM_READWRITE |
                           G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_CORNER,
     g_param_spec_enum ("corner", NULL, NULL,
                        GTK_TYPE_CORNER_TYPE,
                        GTK_CORNER_BOTTOM_RIGHT,
                        G_PARAM_READWRITE |
                        G_PARAM_CONSTRUCT |
                        G_PARAM_STATIC_STRINGS));

  g_object_class_install_property
    (object_class,
     PROP_WINDOW,
     g_param_spec_object ("window", NULL, NULL,
                          GTK_TYPE_WINDOW,
                          G_PARAM_WRITABLE |
                          G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS));

}

/* public API */

/**
 * ar_fullscreen_button_new:
 * @window: a parent #GtkWindow
 * @corner: which screen corner to use
 *
 * Return value: a new #ArFullscreenButton
 */
GtkWidget *
ar_fullscreen_button_new (GtkWindow *parent,
                          GtkCornerType corner)
{
  return g_object_new (AR_TYPE_FULLSCREEN_BUTTON,
                       "type", GTK_WINDOW_POPUP,
                       "transient-for", parent,
                       "window", parent,
                       "corner", corner,
                       NULL);
}

/**
 * ar_fullscreen_button_set_corner:
 * @button: a #ArFullscreenButton
 * @corner: which screen corner to use
 *
 * Repositions the button to the specified screen corner.
 */
void
ar_fullscreen_button_set_corner (ArFullscreenButton *button,
                                 GtkCornerType corner)
{
  ArFullscreenButtonPrivate *priv;

  g_return_if_fail (AR_IS_FULLSCREEN_BUTTON (button));

  priv = button->priv;
  if (priv->corner == corner)
    return;

  set_corner (button, corner);
  g_object_notify (G_OBJECT (button), "corner");
}

/**
 * ar_fullscreen_button_set_active:
 * @button: a #ArFullscreenButton
 * @active: whether to activate the button
 *
 * When active, the button auto-hides after a certain time, and re-shows
 * on button-press-event on the parent window.
 */
void
ar_fullscreen_button_set_active (ArFullscreenButton *button,
                                 gboolean active)
{
  ArFullscreenButtonPrivate *priv;

  g_return_if_fail (AR_IS_FULLSCREEN_BUTTON (button));

  priv = button->priv;
  
  active = active != FALSE;
  if (priv->active == active)
    return;

  set_active (button, active);
  g_object_notify (G_OBJECT (button), "active");
}
