/* gtk-clutter-embed.h: Embeddable ClutterStage
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

#ifndef __GAMES_CLUTTER_EMBED_H__
#define __GAMES_CLUTTER_EMBED_H__

#include <gtk/gtk.h>
#include <clutter/clutter.h>

G_BEGIN_DECLS

#define GAMES_TYPE_CLUTTER_EMBED          (games_clutter_embed_get_type ())
#define GAMES_CLUTTER_EMBED(o)            (G_TYPE_CHECK_INSTANCE_CAST ((o), GAMES_TYPE_CLUTTER_EMBED, GamesClutterEmbed))
#define GAMES_IS_CLUTTER_EMBED(o)         (G_TYPE_CHECK_INSTANCE_TYPE ((o), GAMES_TYPE_CLUTTER_EMBED))
#define GAMES_CLUTTER_EMBED_CLASS(k)      (G_TYPE_CHECK_CLASS_CAST ((k), GAMES_TYPE_CLUTTER_EMBED, GamesClutterEmbedClass))
#define GAMES_IS_CLUTTER_EMBED_CLASS(k)   (G_TYPE_CHECK_CLASS_TYPE ((k), GAMES_TYPE_CLUTTER_EMBED))
#define GAMES_CLUTTER_EMBED_GET_CLASS(o)  (G_TYPE_INSTANCE_GET_CLASS ((o), GAMES_TYPE_CLUTTER_EMBED, GamesClutterEmbedClass))

typedef struct _GamesClutterEmbed         GamesClutterEmbed;
typedef struct _GamesClutterEmbedPrivate  GamesClutterEmbedPrivate;
typedef struct _GamesClutterEmbedClass    GamesClutterEmbedClass;

/**
 * ClutterGamesInitError:
 * @CLUTTER_INIT_ERROR_LAST: Placeholder
 * @CLUTTER_INIT_ERROR_GTK: Unable to initialize GTK+
 *
 * Extension of the #ClutterInitError enumeration for the integration
 * with GTK+
 *
 * Since: 0.8
 */
typedef enum {
  GAMES_CLUTTER_INIT_ERROR_LAST = CLUTTER_INIT_ERROR_INTERNAL,

  GAMES_CLUTTER_INIT_ERROR_GTK  = (GAMES_CLUTTER_INIT_ERROR_LAST - 1)
} ClutterGamesInitError;

/**
 * GamesClutterEmbed:
 *
 * A #GtkWidget containing the default Clutter stage.
 *
 * Since: 0.6
 */
struct _GamesClutterEmbed
{
  /*< private >*/
  GtkWidget parent_instance;

  GamesClutterEmbedPrivate *priv;
};

/**
 * GamesClutterEmbedClass:
 *
 * Base class for #GamesClutterEmbed.
 *
 * Since: 0.6
 */
struct _GamesClutterEmbedClass
{
  /*< private >*/
  GtkWidgetClass parent_class;
};

GType         games_clutter_embed_get_type  (void) G_GNUC_CONST;
GtkWidget *   games_clutter_embed_new       (void);
ClutterActor *games_clutter_embed_get_stage (GamesClutterEmbed *embed);

ClutterInitError games_clutter_init (gint    *argc,
                                     gchar ***argv);
ClutterInitError games_clutter_init_with_args (int          *argc,
                                               char       ***argv,
                                               const char   *parameter_string,
                                               GOptionEntry *entries,
                                               const char   *translation_domain,
                                               GError      **error);
G_END_DECLS

#endif /* __GAMES_CLUTTER_EMBED_H__ */
