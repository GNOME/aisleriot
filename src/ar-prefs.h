/*  
 * Copyright © 2009, 2013 Christian Persch <chpe@gnome.org>
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

#ifndef __AR_PREFS_H__
#define __AR_PREFS_H__

#include <gtk/gtk.h>

#include "window.h"

G_BEGIN_DECLS

#define AR_TYPE_PREFS            (ar_prefs_get_type())     
#define AR_PREFS(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_PREFS, ArPrefs))     
#define AR_PREFS_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  AR_TYPE_PREFS, ArPrefsClass))
#define AR_IS_PREFS(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_PREFS))                      
#define AR_IS_PREFS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  AR_TYPE_PREFS))                      
#define AR_PREFS_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  AR_TYPE_PREFS, ArPrefsClass))

typedef struct _ArPrefs        ArPrefs;
typedef struct _ArPrefsClass   ArPrefsClass;

GType ar_prefs_get_type (void);

GtkWidget *ar_prefs_new (AisleriotWindow *window);

G_END_DECLS

#endif /* __AR_PREFS_H__ */
