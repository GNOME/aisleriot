/* Scroll menu: a scrolling menu (if it's beyond screen size)
 * (C) 2000  Eazel, Inc.
 *
 * Authors:  George Lebl
 */
#ifndef SCROLL_MENU_H
#define SCROLL_MENU_H

#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#endif

#define TYPE_SCROLL_MENU          (scroll_menu_get_type ())
#define SCROLL_MENU(obj)          GTK_CHECK_CAST ((obj), scroll_menu_get_type (), ScrollMenu)
#define SCROLL_MENU_CLASS(klass)  GTK_CHECK_CLASS_CAST ((klass), scroll_menu_get_type (), ScrollMenuClass)
#define IS_SCROLL_MENU(obj)       GTK_CHECK_TYPE ((obj), scroll_menu_get_type ())

typedef struct _ScrollMenu        ScrollMenu;
typedef struct _ScrollMenuClass   ScrollMenuClass;

struct _ScrollMenu
{
	GtkMenu			menu;

	/*< private >*/
	int			offset;
	int			max_offset;

	gboolean		scroll;

	GtkWidget		*up_scroll /* GtkButton */;
	GtkWidget		*down_scroll /* GtkButton */;
	gboolean		in_up;
	gboolean		in_down;

	int			scroll_by;
	guint			scroll_timeout;
};

struct _ScrollMenuClass
{
	GtkMenuClass parent_class;
};

GtkType		scroll_menu_get_type		(void) G_GNUC_CONST;
GtkWidget *	scroll_menu_new			(void);

#ifdef __cplusplus
}
#endif

#endif /* SCROLL_MENU_H */
