/*  
 * Copyright © 2009 Christian Persch <chpe@src.gnome.org>
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

#ifndef __AR_GAME_CHOOSER_H__
#define __AR_GAME_CHOOSER_H__

#include <gtk/gtk.h>

#include "window.h"

G_BEGIN_DECLS

#define AR_TYPE_GAME_CHOOSER            (ar_game_chooser_get_type())     
#define AR_GAME_CHOOSER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_GAME_CHOOSER, ArGameChooser))     
#define AR_GAME_CHOOSER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  AR_TYPE_GAME_CHOOSER, ArGameChooserClass))
#define AR_IS_GAME_CHOOSER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_GAME_CHOOSER))                      
#define AR_IS_GAME_CHOOSER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  AR_TYPE_GAME_CHOOSER))                      
#define AR_GAME_CHOOSER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  AR_TYPE_GAME_CHOOSER, ArGameChooserClass))

typedef struct _ArGameChooser        ArGameChooser;
typedef struct _ArGameChooserClass   ArGameChooserClass;
typedef struct _ArGameChooserPrivate ArGameChooserPrivate;

GType ar_game_chooser_get_type (void);

GtkWidget *ar_game_chooser_new (AisleriotWindow *window);

G_END_DECLS

#endif /* __AR_GAME_CHOOSER_H__ */
