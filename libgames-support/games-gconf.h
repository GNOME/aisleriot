/*
 * games-gconf.h: GConf utility functions. 
 *
 * Copyright (C) 2003 Ross Burton
 *
 * Authors: Ross Burton <ross@burtonini.com>
 */

#ifndef __GAMES_GCONF_H__
#define __GAMES_GCONF_H__

#include <gconf/gconf-client.h>

G_BEGIN_DECLS

gboolean games_gconf_sanity_check_string (GConfClient *client, const gchar* key);

gchar* games_gconf_get_string (GConfClient *client, const gchar* key, const gchar* def);

G_END_DECLS

#endif /* __GAMES_GCONF_H__ */
