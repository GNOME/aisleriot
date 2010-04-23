/*
 * Copyright © 2010 Robert Ancell
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * Authors:
 *      Robert Ancell <robert.ancell@gmail.com>
 */

#include <glib/gi18n.h>

#include "games-fullscreen-action.h"
#include "games-stock.h"

enum {
    PROP_0,
    PROP_WINDOW,
    PROP_IS_FULLSCREEN,
    PROP_VISIBLE_POLICY,
};

struct GamesFullscreenActionPrivate
{
    GtkWindow *window;
    gboolean is_fullscreen;
    GamesFullscreenActionVisiblePolicy visible_policy;
};

G_DEFINE_TYPE (GamesFullscreenAction, games_fullscreen_action, GTK_TYPE_ACTION);

GamesFullscreenAction *
games_fullscreen_action_new (const gchar *name, GtkWindow *window)
{
    return g_object_new (GAMES_TYPE_FULLSCREEN_ACTION, "name", name, "window", window, NULL);
}

static void
update_action (GamesFullscreenAction *action)
{
    gboolean visible = TRUE;
  
    if (action->priv->is_fullscreen) {
        gtk_action_set_stock_id (GTK_ACTION (action), GAMES_STOCK_LEAVE_FULLSCREEN);
        if (action->priv->visible_policy == GAMES_FULLSCREEN_ACTION_VISIBLE_ON_UNFULLSCREEN)
            visible = FALSE;
    }
    else {
        if (action->priv->visible_policy == GAMES_FULLSCREEN_ACTION_VISIBLE_ON_FULLSCREEN)
            visible = FALSE;
        gtk_action_set_stock_id (GTK_ACTION (action), GAMES_STOCK_FULLSCREEN);
    }

    gtk_action_set_visible (GTK_ACTION (action), visible);
}

void
games_fullscreen_action_set_visible_policy (GamesFullscreenAction *action, GamesFullscreenActionVisiblePolicy visible_policy)
{
    g_return_if_fail (GAMES_IS_FULLSCREEN_ACTION (action));

    action->priv->visible_policy = visible_policy;
    update_action (action);
}

GamesFullscreenActionVisiblePolicy
games_fullscreen_action_get_visible_policy (GamesFullscreenAction *action)
{
    g_return_val_if_fail (GAMES_IS_FULLSCREEN_ACTION (action), 0);
    return action->priv->visible_policy;
}

void
games_fullscreen_action_set_is_fullscreen (GamesFullscreenAction *action, gboolean is_fullscreen)
{
    g_return_if_fail (GAMES_IS_FULLSCREEN_ACTION (action));  

    if (is_fullscreen)
        gtk_window_fullscreen (action->priv->window);
    else
        gtk_window_unfullscreen (action->priv->window);
}

gboolean
games_fullscreen_action_get_is_fullscreen (GamesFullscreenAction *action)
{
    g_return_val_if_fail (GAMES_IS_FULLSCREEN_ACTION (action), FALSE);
    return action->priv->is_fullscreen;
}

static gboolean
window_state_event_cb (GtkWidget *widget,
                       GdkEventWindowState *event,
                       GamesFullscreenAction *action)
{
  gboolean is_fullscreen;

  if ((event->changed_mask & GDK_WINDOW_STATE_FULLSCREEN) == 0)
      return FALSE;

  is_fullscreen = (event->new_window_state & GDK_WINDOW_STATE_FULLSCREEN) != 0;  
  if ((action->priv->is_fullscreen && is_fullscreen) || (!action->priv->is_fullscreen && !is_fullscreen))
      return FALSE;

  action->priv->is_fullscreen = is_fullscreen;

  update_action (action);

  g_object_notify (G_OBJECT (action), "is-fullscreen");

  return FALSE;
}

static void
games_fullscreen_action_set_property(GObject      *object,
                                     guint         prop_id,
                                     const GValue *value,
                                     GParamSpec   *pspec)
{
    GamesFullscreenAction *self;

    self = GAMES_FULLSCREEN_ACTION (object);

    switch (prop_id) {
    case PROP_WINDOW:
        self->priv->window = g_object_ref (g_value_get_object (value));
        g_signal_connect (self->priv->window, "window-state-event",
                          G_CALLBACK (window_state_event_cb), self);
        break;
    case PROP_IS_FULLSCREEN:
        games_fullscreen_action_set_is_fullscreen (self, g_value_get_boolean (value));
        break;
    case PROP_VISIBLE_POLICY:
        games_fullscreen_action_set_visible_policy (self, g_value_get_int (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
games_fullscreen_action_get_property(GObject    *object,
                                     guint       prop_id,
                                     GValue     *value,
                                     GParamSpec *pspec)
{
    GamesFullscreenAction *self;

    self = GAMES_FULLSCREEN_ACTION (object);

    switch (prop_id) {
    case PROP_WINDOW:
        g_value_set_object (value, self->priv->window);
        break;
    case PROP_IS_FULLSCREEN:
        g_value_set_boolean (value, games_fullscreen_action_get_is_fullscreen (self));
        break;
    case PROP_VISIBLE_POLICY:
        g_value_set_int (value, games_fullscreen_action_get_visible_policy (self));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
games_fullscreen_action_init (GamesFullscreenAction *self)
{
    self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self, GAMES_TYPE_FULLSCREEN_ACTION, GamesFullscreenActionPrivate);
    update_action (self);
}

static void
games_fullscreen_action_activate (GtkAction *action)
{
    GamesFullscreenAction *self = GAMES_FULLSCREEN_ACTION (action);
    games_fullscreen_action_set_is_fullscreen (self, !self->priv->is_fullscreen);
}

static void
games_fullscreen_action_class_init (GamesFullscreenActionClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GtkActionClass *action_class = GTK_ACTION_CLASS (klass);

    action_class->activate = games_fullscreen_action_activate;
    object_class->set_property = games_fullscreen_action_set_property;
    object_class->get_property = games_fullscreen_action_get_property;

    g_object_class_install_property(object_class,
                                    PROP_WINDOW,
                                    g_param_spec_object("window",
                                                        "window",
                                                        "The window being controlled",
                                                        GTK_TYPE_WINDOW,
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

    g_object_class_install_property(object_class,
                                    PROP_IS_FULLSCREEN,
                                    g_param_spec_boolean("is-fullscreen",
                                                         "is-fullscreen",
                                                         "True if game is fullscreen",
                                                         FALSE,
                                                         G_PARAM_READWRITE));

    g_object_class_install_property(object_class,
                                    PROP_VISIBLE_POLICY,
                                    g_param_spec_boolean("visible-policy",
                                                         "visible-policy",
                                                         "Policy when to show this action",
                                                         GAMES_FULLSCREEN_ACTION_VISIBLE_ALWAYS,
                                                         G_PARAM_READWRITE));

    g_type_class_add_private (klass, sizeof (GamesFullscreenActionPrivate));
}
