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

#include "games-pause-action.h"
#include "games-stock.h"

enum {
    PROP_0,
    PROP_IS_PAUSED
};

struct GamesPauseActionPrivate
{
    gboolean is_paused;
};

G_DEFINE_TYPE (GamesPauseAction, games_pause_action, GTK_TYPE_ACTION);

GamesPauseAction *
games_pause_action_new (const char *name)
{
    return g_object_new (GAMES_PAUSE_ACTION_TYPE, "name", name, NULL);
}

void
games_pause_action_set_is_paused (GamesPauseAction *action, gboolean is_paused)
{
    if ((action->priv->is_paused && is_paused) || (!action->priv->is_paused && !is_paused))
        return;
    action->priv->is_paused = is_paused;
    if (is_paused)
        gtk_action_set_stock_id (GTK_ACTION (action), GAMES_STOCK_RESUME_GAME);
    else
        gtk_action_set_stock_id (GTK_ACTION (action), GAMES_STOCK_PAUSE_GAME);
    g_object_notify (G_OBJECT (action), "is-paused");
}

gboolean
games_pause_action_get_is_paused (GamesPauseAction *action)
{
    return action->priv->is_paused;
}

static void
games_pause_action_set_property(GObject      *object,
                                guint         prop_id,
                                const GValue *value,
                                GParamSpec   *pspec)
{
    GamesPauseAction *self;

    self = GAMES_PAUSE_ACTION (object);

    switch (prop_id) {
    case PROP_IS_PAUSED:
        games_pause_action_set_is_paused (self, g_value_get_boolean (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
games_pause_action_get_property(GObject    *object,
                                guint       prop_id,
                                GValue     *value,
                                GParamSpec *pspec)
{
    GamesPauseAction *self;

    self = GAMES_PAUSE_ACTION (object);

    switch (prop_id) {
    case PROP_IS_PAUSED:
        g_value_set_boolean (value, games_pause_action_get_is_paused (self));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
games_pause_action_init (GamesPauseAction *self)
{
    self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self, GAMES_PAUSE_ACTION_TYPE, GamesPauseActionPrivate);
    gtk_action_set_stock_id (GTK_ACTION (self), GAMES_STOCK_PAUSE_GAME);
}

static void
games_pause_action_activate (GtkAction *action)
{
    GamesPauseAction *self = GAMES_PAUSE_ACTION (action);
    games_pause_action_set_is_paused (self, !self->priv->is_paused);
}

static void
games_pause_action_class_init (GamesPauseActionClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GtkActionClass *action_class = GTK_ACTION_CLASS (klass);

    action_class->activate = games_pause_action_activate;  
    object_class->set_property = games_pause_action_set_property;
    object_class->get_property = games_pause_action_get_property;

    g_object_class_install_property(object_class,
                                    PROP_IS_PAUSED,
                                    g_param_spec_boolean("is-paused",
                                                         "is-paused",
                                                         "True if game is paused",
                                                         FALSE,
                                                         G_PARAM_READWRITE));

    g_type_class_add_private (klass, sizeof (GamesPauseActionPrivate));
}
