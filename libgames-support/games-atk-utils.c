/* 
 * Copyright © 1998, 2001, 2003, 2006 Jonathan Blandford <jrb@alum.mit.edu>
 * Copyright © 2007 Christian Persch
 *
 * This game is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 */

#include <config.h>

#include <gtk/gtk.h>

#include "games-atk-utils.h"

/**
 * games_atk_util_add_atk_relation:
 * @widget:
 * @other:
 * @type:
 *
 * Adds an AtkRelation of type @type to @other into @widget's
 * AtkRelationSet.
 */
void
games_atk_util_add_atk_relation (GtkWidget *widget,
                                 GtkWidget *other,
                                 AtkRelationType type)
{
  AtkRelationSet *set;
  AtkRelation *relation;
  AtkObject *object;

  object = gtk_widget_get_accessible (other);
  set = atk_object_ref_relation_set (gtk_widget_get_accessible (widget));
  relation = atk_relation_new (&object, 1, type);
  atk_relation_set_add (set, relation);
  g_object_unref (relation);
  g_object_unref (set);
}
