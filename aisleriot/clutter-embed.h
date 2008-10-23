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

#ifndef __AISLERIOT_CLUTTER_EMBED_H__
#define __AISLERIOT_CLUTTER_EMBED_H__

#include <gtk/gtk.h>
#include <clutter/clutter.h>

G_BEGIN_DECLS

#define AISLERIOT_TYPE_CLUTTER_EMBED          (aisleriot_clutter_embed_get_type ())
#define AISLERIOT_CLUTTER_EMBED(o)            (G_TYPE_CHECK_INSTANCE_CAST ((o), AISLERIOT_TYPE_CLUTTER_EMBED, AisleriotClutterEmbed))
#define AISLERIOT_IS_CLUTTER_EMBED(o)         (G_TYPE_CHECK_INSTANCE_TYPE ((o), AISLERIOT_TYPE_CLUTTER_EMBED))
#define AISLERIOT_CLUTTER_EMBED_CLASS(k)      (G_TYPE_CHECK_CLASS_CAST ((k), AISLERIOT_TYPE_CLUTTER_EMBED, AisleriotClutterEmbedClass))
#define AISLERIOT_IS_CLUTTER_EMBED_CLASS(k)   (G_TYPE_CHECK_CLASS_TYPE ((k), AISLERIOT_TYPE_CLUTTER_EMBED))
#define AISLERIOT_CLUTTER_EMBED_GET_CLASS(o)  (G_TYPE_INSTANCE_GET_CLASS ((o), AISLERIOT_TYPE_CLUTTER_EMBED, AisleriotClutterEmbedClass))

typedef struct _AisleriotClutterEmbed         AisleriotClutterEmbed;
typedef struct _AisleriotClutterEmbedPrivate  AisleriotClutterEmbedPrivate;
typedef struct _AisleriotClutterEmbedClass    AisleriotClutterEmbedClass;

/**
 * ClutterAisleriotInitError:
 * @CLUTTER_INIT_ERROR_LAST: Placeholder
 * @CLUTTER_INIT_ERROR_GTK: Unable to initialize GTK+
 *
 * Extension of the #ClutterInitError enumeration for the integration
 * with GTK+
 *
 * Since: 0.8
 */
typedef enum {
  AISLERIOT_CLUTTER_INIT_ERROR_LAST = CLUTTER_INIT_ERROR_INTERNAL,

  AISLERIOT_CLUTTER_INIT_ERROR_GTK  = (AISLERIOT_CLUTTER_INIT_ERROR_LAST - 1)
} ClutterAisleriotInitError;

/**
 * AisleriotClutterEmbed:
 *
 * A #GtkWidget containing the default Clutter stage.
 *
 * Since: 0.6
 */
struct _AisleriotClutterEmbed
{
  /*< private >*/
  GtkWidget parent_instance;

  AisleriotClutterEmbedPrivate *priv;
};

/**
 * AisleriotClutterEmbedClass:
 *
 * Base class for #AisleriotClutterEmbed.
 *
 * Since: 0.6
 */
struct _AisleriotClutterEmbedClass
{
  /*< private >*/
  GtkWidgetClass parent_class;

  /* padding for future expansion */
  void (*_clutter_gtk_reserved1) (void);
  void (*_clutter_gtk_reserved2) (void);
  void (*_clutter_gtk_reserved3) (void);
  void (*_clutter_gtk_reserved4) (void);
  void (*_clutter_gtk_reserved5) (void);
  void (*_clutter_gtk_reserved6) (void);
};

GType         aisleriot_clutter_embed_get_type  (void) G_GNUC_CONST;
GtkWidget *   aisleriot_clutter_embed_new       (void);
ClutterActor *aisleriot_clutter_embed_get_stage (AisleriotClutterEmbed *embed);

ClutterInitError aisleriot_clutter_init (gint    *argc,
                                   gchar ***argv);

G_END_DECLS

#endif /* __AISLERIOT_CLUTTER_EMBED_H__ */
