/*
 *clock_widget.h: Clock widget.
 *
 * Copyright © 2001, 2003 Iain Holmes
 *           © 2001 Mark McLoughlin
 *
 * Authors: Iain Holmes <iain@ximian.com>
 *          Mark McLoughlin <mark@skynet.ie>
 */

#ifndef __AR_CLOCK_H__
#define __AR_CLOCK_H__

#include <glib.h>
#include <gtk/gtk.h>
#include <time.h>

G_BEGIN_DECLS

#define AR_TYPE_CLOCK            (ar_clock_get_type ())
#define AR_CLOCK(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), AR_TYPE_CLOCK, ArClock))
#define AR_CLOCK_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), AR_TYPE_CLOCK, ArClockClass))
#define GAMES_IS_CLOCK(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), AR_TYPE_CLOCK))
#define GAMES_IS_CLOCK_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), AR_TYPE_CLOCK))

typedef struct ArClockPrivate ArClockPrivate;

typedef struct {
  GtkLabel label;
  /*< private >*/
  ArClockPrivate *priv;
} ArClock;

typedef struct {
  GtkLabelClass parent_class;
} ArClockClass;

GType      ar_clock_get_type     (void);
GtkWidget *ar_clock_new          (void);
void       ar_clock_start        (ArClock *clock_widget);
gboolean   ar_clock_is_started   (ArClock *clock_widget);
void       ar_clock_stop         (ArClock *clock_widget);
void       ar_clock_reset        (ArClock *clock_widget);
time_t     ar_clock_get_seconds  (ArClock *clock_widget);
void       ar_clock_add_seconds  (ArClock *clock_widget,
                                     time_t seconds);
void       ar_clock_set_update   (ArClock *clock_widget,
                                     gboolean do_update);

G_END_DECLS

#endif /* __AR_CLOCK_H__ */
