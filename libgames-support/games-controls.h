/*
 * games-controls.h: keyboard controls utility functions. 
 *
 * Copyright (C) 2004 Paolo Borelli
 *
 */

#ifndef __GAMES_CONTROLS_H__
#define __GAMES_CONTROLS_H__

G_BEGIN_DECLS

#include "gtk/gtk.h"

#define GAMES_TYPE_CONTROLS_LIST            (games_controls_list_get_type ())
#define GAMES_CONTROLS_LIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CONTROLS_LIST, GamesControlsList))
#define GAMES_CONTROLS_LIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CONTROLS_LIST, GamesControlsListClass))
#define GAMES_IS_CONTROLS_LIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CONTROLS_LIST))
#define GAMES_IS_CONTROLS_LIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CONTROLS_LIST))
#define GAMES_CONTROLS_LIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CONTROLS_LIST, GamesControlsListClass))

typedef struct _GamesControlsList GamesControlsList;
typedef struct _GamesControlsListClass GamesControlsListClass;

struct _GamesControlsList
{
	GtkVBox vbox;

	GtkListStore *store;
	GtkWidget *view;

	GSList * notify_list;

	gboolean dispose_has_run;
};

struct _GamesControlsListClass
{
	GtkVBoxClass parent_class;
};


GType games_controls_list_get_type (void);

GtkWidget *games_controls_list_new (void);

void games_controls_list_add_control (GamesControlsList *list,
                                      const gchar *gconf_key);
void games_controls_list_add_controls (GamesControlsList *list,
                                       const gchar *first_gconf_key, ...);

G_END_DECLS

#endif /* __GAMES_CONTROLS_H__ */
