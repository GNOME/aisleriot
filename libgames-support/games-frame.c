/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * GamesFrame: Create a bold-titled, indented frame
 *
 *     Copyright 2003 William Jon McCann
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

#include <gtk/gtk.h>
#include "games-frame.h"

enum {
  PROP_0,
  PROP_LABEL,
  PROP_INDENT
};

static void games_frame_class_init   (GamesFrameClass *klass);
static void games_frame_init         (GamesFrame      *frame);
static void games_frame_set_property (GObject         *object,
                                      guint            prop_id,
                                      const GValue    *value,
                                      GParamSpec      *pspec);
static void games_frame_get_property (GObject         *object,
                                      guint            prop_id,
                                      GValue          *value,
                                      GParamSpec      *pspec);
static void games_frame_add          (GtkContainer   *container,
                                      GtkWidget      *widget);

#define MAX_INDENT 30
#define MIN_INDENT 0
#define DEFAULT_INDENT 4

static GtkFrameClass *parent_class = NULL;

GType
games_frame_get_type (void)
{
  static GType frame_type = 0;
  
  if (!frame_type)
    {
      static const GTypeInfo frame_info =
      {
	sizeof (GamesFrameClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) games_frame_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (GamesFrame),
	0,		/* n_preallocs */
	(GInstanceInitFunc) games_frame_init,
      };
      
      frame_type =
	g_type_register_static (GTK_TYPE_FRAME, "GamesFrame",
				&frame_info, 0);
    }
  
  return frame_type;
}

static void
games_frame_class_init (GamesFrameClass *class)
{
  GObjectClass *gobject_class;
  GtkFrameClass *frame_class;
  GtkContainerClass *container_class;
  
  parent_class = g_type_class_peek_parent (class);

  gobject_class = (GObjectClass*) class;
  frame_class = (GtkFrameClass*) class;
  container_class = (GtkContainerClass*) class;

  gobject_class->set_property = games_frame_set_property;
  gobject_class->get_property = games_frame_get_property;

  container_class->add = games_frame_add;

  /* FIXME
  g_object_class_install_property (gobject_class,
                                   PROP_INDENT,
                                   g_param_spec_int ("indent",
                                                       _("Horizontal offset"),
                                                       _("X offset of the child in chars"),
                                                       0.0, 100, 4,
                                                       G_PARAM_READABLE | G_PARAM_WRITABLE ));
  */
  g_object_class_install_property (gobject_class,
                                   PROP_INDENT,
                                   g_param_spec_int ("indent",
                                                       "Horizontal offset",
                                                       "X offset of the child in chars",
                                                       MIN_INDENT, MAX_INDENT, DEFAULT_INDENT,
                                                       G_PARAM_READABLE | G_PARAM_WRITABLE ));
}

static void
games_frame_init (GamesFrame *frame)
{
  frame->indent = DEFAULT_INDENT;
  gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);
}

static void
games_frame_set_property (GObject         *object,
                          guint            prop_id,
                          const GValue    *value,
                          GParamSpec      *pspec)
{
  GamesFrame *frame = GAMES_FRAME (object);
  
  switch (prop_id)
    {
      /* g_object_notify is handled by the _frame_set function */
    case PROP_LABEL:
      games_frame_set_label (frame, g_value_get_string (value));
      break;
     case PROP_INDENT:
      games_frame_set (frame, g_value_get_int (value));
      break;
    default:
       G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
games_frame_get_property (GObject         *object,
                          guint            prop_id,
                          GValue          *value,
                          GParamSpec      *pspec)
{
  GamesFrame *frame = GAMES_FRAME (object);
  
  switch (prop_id)
    {
    case PROP_INDENT:
      g_value_set_int (value, frame->indent);
      break;
    default:
       G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

GtkWidget*
games_frame_new (const gchar *label)
{
  GtkWidget *frame;
  gchar *markup;

  frame = g_object_new (TYPE_GAMES_FRAME, NULL);

  markup = g_strdup_printf ("<span weight=\"bold\">%s</span>", label);
  gtk_frame_set_label (GTK_FRAME (frame), markup);
  g_free (markup);
  gtk_label_set_use_markup (GTK_LABEL (gtk_frame_get_label_widget (GTK_FRAME (frame))),
                            TRUE);
  gtk_misc_set_alignment (GTK_MISC (gtk_frame_get_label_widget (GTK_FRAME (frame))),
                          0, 0.5);
  gtk_container_set_border_width (GTK_CONTAINER (frame), 12);

  return frame;
}

void
games_frame_set (GamesFrame *frame,
                 gint          indent)
{
  g_return_if_fail (IS_GAMES_FRAME (frame));
  
  indent = CLAMP (indent, MIN_INDENT, MAX_INDENT);
  
  if (   (frame->indent != indent))
    {
      g_object_freeze_notify (G_OBJECT (frame));

      if (frame->indent != indent)
        {
          frame->indent = indent;
          g_object_notify (G_OBJECT (frame), "indent");
        }
      g_object_thaw_notify (G_OBJECT (frame));

      gtk_widget_queue_resize (GTK_WIDGET (frame));
    }
}

static void
games_frame_add (GtkContainer *container,
                 GtkWidget    *child)
{
  GtkBin *bin = GTK_BIN (container);
  GtkWidget *hbox;
  GtkWidget *space_label;
  gchar *label_string;
  gint indent;

  g_return_if_fail (GTK_IS_WIDGET (child));

  if (bin->child != NULL)
    {
      g_warning ("Attempting to add a widget with type %s to a %s, "
                 "but as a GtkBin subclass a %s can only contain one widget at a time; "
                 "it already contains a widget of type %s",
                 g_type_name (G_OBJECT_TYPE (child)),
                 g_type_name (G_OBJECT_TYPE (bin)),
                 g_type_name (G_OBJECT_TYPE (bin)),
                 g_type_name (G_OBJECT_TYPE (bin->child)));
      return;
    }

  hbox = gtk_hbox_new (FALSE, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (hbox), 6);
  indent = GAMES_FRAME (container)->indent;

  label_string = g_strnfill (indent, ' ');
  space_label = gtk_label_new (label_string);
  g_free (label_string);
  gtk_box_pack_start (GTK_BOX(hbox), space_label, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX(hbox), child, FALSE, FALSE, 0);

  gtk_widget_unparent (GTK_WIDGET (hbox));
  gtk_widget_set_parent (GTK_WIDGET (hbox), GTK_WIDGET (bin));
  bin->child = hbox;
}

void
games_frame_set_label (GamesFrame *frame,
                       const gchar *label)
{
  gchar *markup;

  g_return_if_fail (GTK_IS_FRAME (frame));

  if (!label)
    {
      gtk_frame_set_label_widget (frame, NULL);
    }
  else
    {
      markup = g_strdup_printf ("<span weight=\"bold\">%s</span>", label);
      GtkWidget *child = gtk_label_new (markup);
      g_free (markup);
      gtk_label_set_use_markup (GTK_LABEL (child), TRUE);
      gtk_misc_set_alignment (GTK_MISC (child), 0, 0.5);
      gtk_widget_show (child);
      gtk_frame_set_label_widget (frame, child);
    }
}

