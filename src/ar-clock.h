/*
 *clock_widget.h: Clock widget.
 *
 * Copyright © 2001, 2003 Iain Holmes
 *           © 2001 Mark McLoughlin
 *
 * Authors: Iain Holmes <iain@ximian.com>
 *          Mark McLoughlin <mark@skynet.ie>
 */

#pragma once

#include <glib.h>
#include <gtk/gtk.h>
#include <time.h>

G_BEGIN_DECLS

#define AR_TYPE_CLOCK (ar_clock_get_type ())
G_DECLARE_FINAL_TYPE  (ArClock, ar_clock, AR, CLOCK, GtkLabel)

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
