/*
 * Copyright © 2008 Neil Roberts
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

#include "baize.h"

#include <gdk-pixbuf/gdk-pixbuf.h>

#include <cogl/cogl.h>

#include <libgames-support/games-runtime.h>

/* Special version of ClutterTexture that repeats the texture to fill
   the entire stage. This is used to paint the baize background */

static void aisleriot_baize_paint (ClutterActor *actor);

static void
load_background (AisleriotBaize *baize)
{
  GError *error = NULL;
  GdkPixbuf *pixbuf;
  char *path;

  path = games_runtime_get_file (GAMES_RUNTIME_PIXMAP_DIRECTORY, "baize.png");

  pixbuf = gdk_pixbuf_new_from_file (path, &error);
  g_free (path);
  if (error) {
    g_warning ("Failed to load the baize pixbuf: %s\n", error->message);
    g_error_free (error);
    return;
  }

  g_assert (pixbuf != NULL);

  clutter_texture_set_from_rgb_data (CLUTTER_TEXTURE (baize),
                                     gdk_pixbuf_get_pixels (pixbuf),
                                     gdk_pixbuf_get_has_alpha (pixbuf),
                                     gdk_pixbuf_get_width (pixbuf),
                                     gdk_pixbuf_get_height (pixbuf),
                                     gdk_pixbuf_get_rowstride (pixbuf),
                                     gdk_pixbuf_get_has_alpha (pixbuf) ? 4 : 3,
                                     0,
                                     &error);
  if (error) {
    g_warning ("Failed to set texture from pixbuf: %s", error->message);
    g_error_free (error);
  }

  g_object_unref (pixbuf);
}

G_DEFINE_TYPE (AisleriotBaize, aisleriot_baize, CLUTTER_TYPE_TEXTURE);

static void
aisleriot_baize_class_init (AisleriotBaizeClass *klass)
{
  ClutterActorClass *actor_class = (ClutterActorClass *) klass;

  actor_class->paint = aisleriot_baize_paint;
}

static void
aisleriot_baize_init (AisleriotBaize *baize)
{
  load_background (baize);
}

ClutterActor *
aisleriot_baize_new (void)
{
  return g_object_new (AISLERIOT_TYPE_BAIZE, NULL);
}

static void
aisleriot_baize_paint (ClutterActor *actor)
{
  ClutterActor *stage;
  CoglHandle tex;
  ClutterGeometry stage_geom;
  guint tex_width, tex_height;

  if ((stage = clutter_actor_get_stage (actor)) == NULL)
    return;

  if ((tex = clutter_texture_get_cogl_texture (CLUTTER_TEXTURE (actor)))
      == COGL_INVALID_HANDLE)
    return;

  tex_width = cogl_texture_get_width (tex);
  tex_height = cogl_texture_get_height (tex);

  if (tex_width < 1 || tex_height < 1)
    return;

  clutter_actor_get_allocation_geometry (stage, &stage_geom);

  /* Repeat the texture to fill the size of the stage */
  cogl_set_source_texture (tex);
  cogl_rectangle_with_texture_coords (0, 0, stage_geom.width, stage_geom.height,
                                      0, 0,
                                      (gfloat) stage_geom.width / tex_width,
                                      (gfloat) stage_geom.height / tex_height);
}
