/*
 *  Copyright © 2008 Neil Roberts
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <config.h>

#include <clutter/clutter.h>
#include <cogl/cogl.h>

#include "baize.h"

/* Special version of ClutterTexture that repeats the texture to fill
   the entire stage. This is used to paint the baize background */

static void aisleriot_baize_paint (ClutterActor *actor);

G_DEFINE_TYPE (AisleriotBaize, aisleriot_baize, CLUTTER_TYPE_TEXTURE);

static void
aisleriot_baize_class_init (AisleriotBaizeClass *klass)
{
  ClutterActorClass *actor_class = (ClutterActorClass *) klass;

  actor_class->paint = aisleriot_baize_paint;
}

static void
aisleriot_baize_init (AisleriotBaize *self)
{
}

ClutterActor *
aisleriot_baize_new (void)
{
  ClutterActor *self = g_object_new (AISLERIOT_TYPE_BAIZE, NULL);

  return self;
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
  cogl_texture_rectangle (tex, 0, 0,
                          CLUTTER_INT_TO_FIXED (stage_geom.width),
                          CLUTTER_INT_TO_FIXED (stage_geom.height),
                          0, 0,
                          CLUTTER_INT_TO_FIXED (stage_geom.width)
                          / tex_width,
                          CLUTTER_INT_TO_FIXED (stage_geom.height)
                          / tex_height);
}
