/*
 *clock_widget.h: Clock widget.
 *
 * Copyright © 2001, 2003 Iain Holmes
 *           © 2001 Mark McLoughlin
 *
 * Authors: Iain Holmes <iain@ximian.com>
 *          Mark McLoughlin <mark@skynet.ie>
 */

#ifndef __GAMES_CLOCK_H__
#define __GAMES_CLOCK_H__

#include <glib.h>
#include <gtk/gtk.h>
#include <time.h>

G_BEGIN_DECLS

#define GAMES_TYPE_CLOCK            (games_clock_get_type ())
#define GAMES_CLOCK(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GAMES_TYPE_CLOCK, GamesClock))
#define GAMES_CLOCK_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GAMES_TYPE_CLOCK, GamesClockClass))
#define GAMES_IS_CLOCK(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GAMES_TYPE_CLOCK))
#define GAMES_IS_CLOCK_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GAMES_TYPE_CLOCK))

typedef struct GamesClockPrivate GamesClockPrivate;

typedef struct {
  GtkLabel label;
  /*< private >*/
  GamesClockPrivate *priv;
} GamesClock;

typedef struct {
  GtkLabelClass parent_class;
} GamesClockClass;

GType      games_clock_get_type     (void);
GtkWidget *games_clock_new          (void);
void       games_clock_start        (GamesClock *clock_widget);
gboolean   games_clock_is_started   (GamesClock *clock_widget);
void       games_clock_stop         (GamesClock *clock_widget);
void       games_clock_reset        (GamesClock *clock_widget);
time_t     games_clock_get_seconds  (GamesClock *clock_widget);
void       games_clock_add_seconds  (GamesClock *clock_widget,
                                     time_t seconds);
void       games_clock_set_update   (GamesClock *clock_widget,
                                     gboolean do_update);

G_END_DECLS

#endif /* __GAMES_CLOCK_H__ */
