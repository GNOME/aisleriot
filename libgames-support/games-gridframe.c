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

#include "games-gridframe.h"

static void
games_grid_frame_size_request (GtkWidget * widget,
			       GtkRequisition *requisition)
{
  GtkWidget * child = GTK_BIN (widget)->child;

  requisition->width = 1;
  requisition->height = 1;
  
  if (child && GTK_WIDGET_VISIBLE (child)) {
    gtk_widget_size_request (child, requisition);
  }
}

static void
games_grid_frame_size_allocate (GtkWidget * widget,
				GtkAllocation *allocation)
{
  GamesGridFrame * frame = GAMES_GRID_FRAME (widget);
  GtkWidget * child = GTK_BIN(widget)->child;
  GtkAllocation child_allocation;
  gint xsize, ysize, size;

  widget->allocation = *allocation;
  
  xsize = MAX (1, allocation->width/frame->xmult);
  ysize = MAX (1, allocation->height/frame->ymult);

  size = MIN (xsize, ysize);

  child_allocation.width = size*frame->xmult;
  child_allocation.x = (allocation->width - child_allocation.width)/2
    + allocation->x;
  child_allocation.height = size*frame->ymult;
  child_allocation.y = (allocation->height - child_allocation.height)/2
    + allocation->y;

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
  GtkWidgetClass *widget_class;

  widget_class = GTK_WIDGET_CLASS (class);

  widget_class->size_allocate = games_grid_frame_size_allocate;
  widget_class->size_request = games_grid_frame_size_request;
}

static void
games_grid_frame_init (GamesGridFrame * frame)
{
  frame->xmult = 1;
  frame->ymult = 1;
}

GType
games_grid_frame_get_type (void)
{
  static GType type = 0;

  if (!type) {
    static const GTypeInfo info = {
      sizeof (GamesGridFrameClass),
      NULL,
      NULL,
      (GClassInitFunc) games_grid_frame_class_init,
      NULL,
      NULL,
      sizeof (GamesGridFrame),
      0,
      (GInstanceInitFunc) games_grid_frame_init,
    };

    type = g_type_register_static (GTK_TYPE_BIN, "GamesGridFrame",
				   &info, 0);
  }

  return type;
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
