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

void
games_grid_frame_set (GamesGridFrame * frame, gint newxmult, gint newymult)
{
  if (newxmult > 0)
    frame->xmult = newxmult;
  if (newymult > 0)
    frame->ymult = newymult;

  gtk_widget_queue_resize (GTK_WIDGET (frame));
}

void
games_grid_frame_set_padding (GamesGridFrame * frame, gint newxpadding,
			      gint newypadding)
{
  if (newxpadding >= 0)
    frame->xpadding = newxpadding;

  if (newypadding >= 0)
    frame->ypadding = newypadding;

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

  frame->xalign = xalign;
  frame->yalign = yalign;

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
				    frame->yalign);
    break;
  case PROP_Y_ALIGN:
    games_grid_frame_set_alignment (frame, frame->xalign,
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
    g_value_set_int (value, frame->xpadding);
    break;
  case PROP_Y_PADDING:
    g_value_set_int (value, frame->ypadding);
    break;
  case PROP_X_ALIGN:
    g_value_set_float (value, frame->xalign);
    break;
  case PROP_Y_ALIGN:
    g_value_set_float (value, frame->yalign);
    break;
  case PROP_WIDTH:
    g_value_set_int (value, frame->xmult);
    break;
  case PROP_HEIGHT:
    g_value_set_int (value, frame->ymult);
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    break;
  }
}

static void
games_grid_frame_size_request (GtkWidget * widget,
			       GtkRequisition * requisition)
{
  GtkWidget *child = GTK_BIN (widget)->child;

  requisition->width = 1;
  requisition->height = 1;

  if (child && GTK_WIDGET_VISIBLE (child)) {
    gtk_widget_size_request (child, requisition);
  }
}

static void
games_grid_frame_size_allocate (GtkWidget * widget,
				GtkAllocation * allocation)
{
  GamesGridFrame *frame = GAMES_GRID_FRAME (widget);
  GtkWidget *child = GTK_BIN (widget)->child;
  GtkAllocation child_allocation;
  gint xsize, ysize, size;

  widget->allocation = *allocation;

  xsize = MAX (1, (allocation->width - frame->xpadding) / frame->xmult);
  ysize = MAX (1, (allocation->height - frame->ypadding) / frame->ymult);

  size = MIN (xsize, ysize);

  child_allocation.width = size * frame->xmult + frame->xpadding;
  child_allocation.height = size * frame->ymult + frame->ypadding;

  child_allocation.x =
    (allocation->width - child_allocation.width) * frame->xalign +
    allocation->x;
  child_allocation.y =
    (allocation->height - child_allocation.height) * frame->yalign +
    allocation->y;

  if (GTK_WIDGET_MAPPED (widget) &&
      (child_allocation.x != frame->old_allocation.x ||
       child_allocation.y != frame->old_allocation.y ||
       child_allocation.width != frame->old_allocation.width ||
       child_allocation.height != frame->old_allocation.height))
    gdk_window_invalidate_rect (widget->window, &widget->allocation, FALSE);

  if (child && GTK_WIDGET_VISIBLE (child))
    gtk_widget_size_allocate (child, &child_allocation);

  frame->old_allocation = child_allocation;
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
  widget_class->size_request = games_grid_frame_size_request;

  g_object_class_install_property (object_class, PROP_X_PADDING,
				   g_param_spec_int ("x_padding",
						     _("X Padding"),
						     _
						     ("Extra space to add to the width allocation."),
						     0, G_MAXINT, 0,
						     G_PARAM_READABLE |
						     G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_Y_PADDING,
				   g_param_spec_int ("y_padding",
						     _("X Padding"),
						     _
						     ("Extra space to add to the height allocation."),
						     0, G_MAXINT, 0,
						     G_PARAM_READABLE |
						     G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_WIDTH,
				   g_param_spec_int ("width_multiple",
						     _("Width Multiple"),
						     _
						     ("What multiple to constrain the width to."),
						     1, G_MAXINT, 1,
						     G_PARAM_READABLE |
						     G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_HEIGHT,
				   g_param_spec_int ("height_multiple",
						     _("Height Multiple"),
						     _
						     ("What multiple to constrain the height to."),
						     1, G_MAXINT, 1,
						     G_PARAM_READABLE |
						     G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_X_ALIGN,
				   g_param_spec_float ("xalign", _("X align"),
						       _
						       ("The horizontal alignment, from 0 (left) to 1 (right)"),
						       0.0, 1.0, 0.5,
						       G_PARAM_READABLE |
						       G_PARAM_WRITABLE));
  g_object_class_install_property (object_class, PROP_Y_ALIGN,
				   g_param_spec_float ("yalign", _("Y align"),
						       _
						       ("The vertical alignment, from 0 (top) to 1 (bottom)"),
						       0.0, 1.0, 0.5,
						       G_PARAM_READWRITE |
						       G_PARAM_WRITABLE));
}

static void
games_grid_frame_init (GamesGridFrame * frame)
{
  frame->xmult = 1;
  frame->ymult = 1;

  frame->xalign = 0.5;
  frame->yalign = 0.5;
}

GtkWidget *
games_grid_frame_new (gint width, gint height)
{
  GamesGridFrame *frame;

  frame = g_object_new (GAMES_TYPE_GRID_FRAME, NULL);

  frame->xmult = MAX (width, 1);
  frame->ymult = MAX (height, 1);

  return GTK_WIDGET (frame);
}

/* EOF */
