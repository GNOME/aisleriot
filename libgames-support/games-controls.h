/*
 * games-controls.h: keyboard controls utility functions. 
 *
 * Copyright © 2004 Paolo Borelli
 *
 */

#ifndef __GAMES_CONTROLS_H__
#define __GAMES_CONTROLS_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GAMES_TYPE_CONTROLS_LIST            (games_controls_list_get_type ())
#define GAMES_CONTROLS_LIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CONTROLS_LIST, GamesControlsList))
#define GAMES_CONTROLS_LIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CONTROLS_LIST, GamesControlsListClass))
#define GAMES_IS_CONTROLS_LIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CONTROLS_LIST))
#define GAMES_IS_CONTROLS_LIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CONTROLS_LIST))
#define GAMES_CONTROLS_LIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GAMES_TYPE_CONTROLS_LIST, GamesControlsListClass))

typedef struct _GamesControlsList GamesControlsList;
typedef struct _GamesControlsListClass GamesControlsListClass;

struct _GamesControlsList {
  GtkVBox vbox;

  GtkListStore *store;
  GtkWidget *view;

  gchar *conf_group;
  gulong notify_handler_id;
};

struct _GamesControlsListClass {
  GtkVBoxClass parent_class;
};

GType games_controls_list_get_type (void);

GtkWidget *games_controls_list_new (const gchar *conf_group);

void games_controls_list_add_control (GamesControlsList * list,
				      const gchar * conf_key,
                                      const gchar * label,
                                      guint default_keyval);
void games_controls_list_add_controls (GamesControlsList * list,
				       const gchar * first_conf_key, ...);

G_END_DECLS
#endif /* __GAMES_CONTROLS_H__ */
