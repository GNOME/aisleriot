/*  
 * Copyright © 2009 Christian Persch <chpe@gnome.org>
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

#ifndef __AR_FULLSCREEN_BUTTON_H__
#define __AR_FULLSCREEN_BUTTON_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define AR_TYPE_FULLSCREEN_BUTTON            (ar_fullscreen_button_get_type())     
#define AR_FULLSCREEN_BUTTON(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_FULLSCREEN_BUTTON, ArFullscreenButton))     
#define AR_FULLSCREEN_BUTTON_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  AR_TYPE_FULLSCREEN_BUTTON, ArFullscreenButtonClass))
#define AR_IS_FULLSCREEN_BUTTON(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_FULLSCREEN_BUTTON))                      
#define AR_IS_FULLSCREEN_BUTTON_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  AR_TYPE_FULLSCREEN_BUTTON))                      
#define AR_FULLSCREEN_BUTTON_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  AR_TYPE_FULLSCREEN_BUTTON, ArFullscreenButtonClass))

typedef struct _ArFullscreenButton        ArFullscreenButton;
typedef struct _ArFullscreenButtonClass   ArFullscreenButtonClass;
typedef struct _ArFullscreenButtonPrivate ArFullscreenButtonPrivate;

struct _ArFullscreenButton
{
  GtkWindow parent;

  /*< private >*/
  ArFullscreenButtonPrivate *priv;
};

struct _ArFullscreenButtonClass
{
  GtkWindowClass parent_class;
};

GType ar_fullscreen_button_get_type (void);

GtkWidget* ar_fullscreen_button_new (GtkWindow *parent,
                                     GtkCornerType corner);

void ar_fullscreen_button_set_corner (ArFullscreenButton *button,
                                      GtkCornerType corner);

void ar_fullscreen_button_set_active (ArFullscreenButton *button,
                                      gboolean active);

G_END_DECLS

#endif /* __AR_FULLSCREEN_BUTTON_H__ */
