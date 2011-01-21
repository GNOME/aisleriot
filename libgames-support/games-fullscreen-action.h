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

#ifndef __GAMES_FULLSCREEN_ACTION_H__
#define __GAMES_FULLSCREEN_ACTION_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GAMES_TYPE_FULLSCREEN_ACTION    (games_fullscreen_action_get_type ())
#define GAMES_FULLSCREEN_ACTION(obj)    (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_FULLSCREEN_ACTION, GamesFullscreenAction))
#define GAMES_IS_FULLSCREEN_ACTION(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_FULLSCREEN_ACTION))

typedef struct GamesFullscreenActionPrivate GamesFullscreenActionPrivate;

typedef struct
{
  GtkAction parent_instance;
  /*< private >*/
  GamesFullscreenActionPrivate *priv;
} GamesFullscreenAction;

typedef struct
{
  GtkActionClass parent_class;
} GamesFullscreenActionClass;

typedef enum
{
  GAMES_FULLSCREEN_ACTION_VISIBLE_ALWAYS,
  GAMES_FULLSCREEN_ACTION_VISIBLE_ON_FULLSCREEN,
  GAMES_FULLSCREEN_ACTION_VISIBLE_ON_UNFULLSCREEN
} GamesFullscreenActionVisiblePolicy;

GType                              games_fullscreen_action_get_type           (void);
GamesFullscreenAction             *games_fullscreen_action_new                (const gchar *name,
                                                                               GtkWindow *window);
void                               games_fullscreen_action_set_visible_policy (GamesFullscreenAction *action,
                                                                               GamesFullscreenActionVisiblePolicy visible_policy);
GamesFullscreenActionVisiblePolicy games_fullscreen_action_get_visible_policy (GamesFullscreenAction *action);
void                               games_fullscreen_action_set_is_fullscreen  (GamesFullscreenAction *action,
                                                                               gboolean is_fullscreen);
gboolean                           games_fullscreen_action_get_is_fullscreen  (GamesFullscreenAction *action);

G_END_DECLS

#endif /* __GAMES_FULLSCREEN_ACTION_H__ */
