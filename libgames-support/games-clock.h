/*
 * clock.c: Clock widget.
 *
 * Copyright (C) 2001 Iain Holmes
 * Authors: Iain Holmes <iain@ximian.com>
 */

#ifndef __CLOCK_H__
#define __CLOCK_H__

#include <time.h>
#include <gtk/gtklabel.h>

#define GTK_TYPE_CLOCK			(clock_get_type ())
#define CLOCK(obj)			(GTK_CHECK_CAST ((obj), GTK_TYPE_CLOCK, Clock))
#define CLOCK_CLASS(klass)		(GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_CLOCK, ClockClass))
#define IS_CLOCK(obj)			(GTK_CHECK_TYPE ((obj), GTK_TYPE_CLOCK))
#define IS_CLOCK_CLASS(klass)   	(GTK_CHECK_CLASS_TYPE ((obj), GTK_TYPE_CLOCK))
#define CLOCK_GET_CLASS(obj)    	(GTK_CHECK_GET_CLASS ((obj), GTK_TYPE_CLOCK, ClockClass))


G_BEGIN_DECLS		
typedef struct _Clock {
	GtkLabel label;

	guint timer_id;

	time_t seconds, stopped;
} Clock;

typedef struct _ClockClass {
	GtkLabelClass parent_class;
} ClockClass;

GType clock_get_type (void);
GtkWidget *clock_new (void);
void clock_start (Clock *clock);
void clock_stop (Clock *clock);
void clock_set_seconds (Clock *clock,
			time_t seconds);

G_END_DECLS

#endif
