/* games-gridframe.c: Create a container that guarantees that the internal
 *                    allocated space is a fixed multiple of an integer. 
 *
 * Copyright 2004 by Callum McKenzie
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
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* A lot of this was written by following the sorce for GtkFrame and
 * GtkAspectFrame. */

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "games-gridframe.h"

enum {
  PROP_0,
  PROP_X_PADDING,
  PROP_Y_PADDING,
  PROP_WIDTH,
  PROP_HEIGHT,
  PROP_X_ALIGN,
  PROP_Y_ALIGN
};

G_DEFINE_TYPE (GamesGridFrame, games_grid_frame,GTK_TYPE_BIN)

struct GamesGridFramePrivate {
  gint xmult;
  gint ymult;

  gint xpadding;
  gint ypadding;

  gfloat xalign;
  gfloat yalign;

  GtkAllocation old_allocation;
};

void
games_grid_frame_set (GamesGridFrame * frame, gint newxmult, gint newymult)
{
  if (newxmult > 0)
    frame->priv->xmult = newxmult;
  if (newymult > 0)
    frame->priv->ymult = newymult;

  gtk_widget_queue_resize (GTK_WIDGET (frame));
}

void
games_grid_frame_set_padding (GamesGridFrame * frame, gint newxpadding,
			      gint newypadding)
{
  if (newxpadding >= 0)
    frame->priv->xpadding = newxpadding;

  if (newypadding >= 0)
    frame->priv->ypadding = newypadding;

  gtk_widget_queue_resize (GTK_WIDGET (frame));
}


void
games_grid_frame_set_alignment (GamesGridFrame * frame, gfloat xalign,
				gfloat yalign)
{
  if (xalign < 0.0)
    xalign = 0.0;
  else if (xalign > 1.0)
    xalign = 1.0;

  if (yalign < 0.0)
    yalign = 0.0;
  else if (yalign > 1.0)
    yalign = 1.0;

  frame->priv->xalign = xalign;
  frame->priv->yalign = yalign;

  gtk_widget_queue_resize (GTK_WIDGET (frame));
}

static void
games_grid_frame_set_property (GObject * object, guint prop_id,
			       const GValue * value, GParamSpec * pspec)
{
  GamesGridFrame *frame = GAMES_GRID_FRAME (object);

  switch (prop_id) {
  case PROP_X_PADDING:
    games_grid_frame_set_padding (frame, g_value_get_int (value), -1);
    break;
  case PROP_Y_PADDING:
    games_grid_frame_set_padding (frame, -1, g_value_get_int (value));
    break;
  case PROP_X_ALIGN:
    games_grid_frame_set_alignment (frame, g_value_get_float (value),
				    frame->priv->yalign);
    break;
  case PROP_Y_ALIGN:
    games_grid_frame_set_alignment (frame, frame->priv->xalign,
				    g_value_get_float (value));
    break;
  case PROP_WIDTH:
    games_grid_frame_set (frame, g_value_get_int (value), -1);
    break;
  case PROP_HEIGHT:
    games_grid_frame_set (frame, -1, g_value_get_int (value));
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    break;
  }
}

static void
games_grid_frame_get_property (GObject * object, guint prop_id,
			       GValue * value, GParamSpec * pspec)
{
  GamesGridFrame *frame = GAMES_GRID_FRAME (object);

  switch (prop_id) {
  case PROP_X_PADDING:
    g_value_set_int (value, frame->priv->xpadding);
    break;
  case PROP_Y_PADDING:
    g_value_set_int (value, frame->priv->ypadding);
    break;
  case PROP_X_ALIGN:
    g_value_set_float (value, frame->priv->xalign);
    break;
  case PROP_Y_ALIGN:
    g_value_set_float (value, frame->priv->yalign);
    break;
  case PROP_WIDTH:
    g_value_set_int (value, frame->priv->xmult);
    break;
  case PROP_HEIGHT:
    g_value_set_int (value, frame->priv->ymult);
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    break;
  }
}

static void
games_grid_frame_size_allocate (GtkWidget * widget,
				GtkAllocation * allocation)
{
  GamesGridFrame *frame = GAMES_GRID_FRAME (widget);
  GtkWidget *child = gtk_bin_get_child (GTK_BIN (widget));
  GtkAllocation child_allocation;
  gint xsize, ysize, size;

  gtk_widget_set_allocation (widget, allocation);

  xsize = MAX (1, (allocation->width - frame->priv->xpadding) / frame->priv->xmult);
  ysize = MAX (1, (allocation->height - frame->priv->ypadding) / frame->priv->ymult);

  size = MIN (xsize, ysize);

  child_allocation.width = size * frame->priv->xmult + frame->priv->xpadding;
  child_allocation.height = size * frame->priv->ymult + frame->priv->ypadding;

  child_allocation.x =
    (allocation->width - child_allocation.width) * frame->priv->xalign +
    allocation->x;
  child_allocation.y =
    (allocation->height - child_allocation.height) * frame->priv->yalign +
    allocation->y;

  if (gtk_widget_get_mapped (widget) &&
      (child_allocation.x != frame->priv->old_allocation.x ||
       child_allocation.y != frame->priv->old_allocation.y ||
       child_allocation.width != frame->priv->old_allocation.width ||
       child_allocation.height != frame->priv->old_allocation.height))
    gdk_window_invalidate_rect (gtk_widget_get_window (widget), allocation, FALSE);

  if (child && gtk_widget_get_visible (child))
    gtk_widget_size_allocate (child, &child_allocation);

  frame->priv->old_allocation = child_allocation;
}

static void
games_grid_frame_class_init (GamesGridFrameClass * class)
{
  GObjectClass *object_class;
  GtkWidgetClass *widget_class;

  object_class = G_OBJECT_CLASS (class);
  widget_class = GTK_WIDGET_CLASS (class);

  object_class->set_property = games_grid_frame_set_property;
  object_class->get_property = games_grid_frame_get_property;

  widget_class->size_allocate = games_grid_frame_size_allocate;

  g_type_class_add_private (object_class, sizeof (GamesGridFramePrivate));  

  g_object_class_install_property (object_class, PROP_X_PADDING,
				   g_param_spec_int ("x_padding", NULL, NULL,
						     0, G_MAXINT, 0,
						     G_PARAM_READABLE |
						     G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_Y_PADDING,
                                   g_param_spec_int ("y_padding", NULL, NULL,
						     0, G_MAXINT, 0,
						     G_PARAM_READABLE |
						     G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_WIDTH,
                                   g_param_spec_int ("width_multiple",  NULL, NULL,
						     1, G_MAXINT, 1,
						     G_PARAM_READABLE |
						     G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_HEIGHT,
                                   g_param_spec_int ("height_multiple", NULL, NULL,
						     1, G_MAXINT, 1,
						     G_PARAM_READABLE |
						     G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_X_ALIGN,
                                   g_param_spec_float ("xalign",  NULL, NULL,
						       0.0, 1.0, 0.5,
						       G_PARAM_READABLE |
						       G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_Y_ALIGN,
                                   g_param_spec_float ("yalign",  NULL, NULL,
						       0.0, 1.0, 0.5,
						       G_PARAM_READWRITE |
						       G_PARAM_WRITABLE));
}

static void
games_grid_frame_init (GamesGridFrame * frame)
{
  frame->priv = G_TYPE_INSTANCE_GET_PRIVATE (frame, GAMES_TYPE_GRID_FRAME, GamesGridFramePrivate);

  frame->priv->xmult = 1;
  frame->priv->ymult = 1;

  frame->priv->xalign = 0.5;
  frame->priv->yalign = 0.5;
}

GtkWidget *
games_grid_frame_new (gint width, gint height)
{
  GamesGridFrame *frame;

  frame = g_object_new (GAMES_TYPE_GRID_FRAME, NULL);

  frame->priv->xmult = MAX (width, 1);
  frame->priv->ymult = MAX (height, 1);

  return GTK_WIDGET (frame);
}

/* EOF */
