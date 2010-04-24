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

#ifndef __GAMES_PAUSE_ACTION_H__
#define __GAMES_PAUSE_ACTION_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GAMES_PAUSE_ACTION_TYPE  (games_pause_action_get_type ())
#define GAMES_PAUSE_ACTION(obj)  (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_PAUSE_ACTION_TYPE, GamesPauseAction))

typedef struct GamesPauseActionPrivate GamesPauseActionPrivate;

typedef struct
{
    GtkAction parent_instance;
    GamesPauseActionPrivate *priv;
} GamesPauseAction;

typedef struct
{
    GtkActionClass parent_class;

    void (*state_changed)(GamesPauseAction *action);
} GamesPauseActionClass;

GType games_pause_action_get_type (void);

GamesPauseAction *games_pause_action_new (const char *name);

void games_pause_action_set_is_paused (GamesPauseAction *action, gboolean is_paused);

gboolean games_pause_action_get_is_paused (GamesPauseAction *action);

G_END_DECLS

#endif /* __GAMES_PAUSE_ACTION_H__ */
