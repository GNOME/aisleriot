/*
 * clock.c: 
 *
 * Copyright (C) 2001 Iain Holmes
 * Authors: Iain Holmes <iain@ximian.com>
 */

#include <glib.h>
#include "clock.h"

static GtkLabelClass *parent_class = NULL;

static void
finalize (GObject *object)
{
	Clock *clock;

	clock = (Clock *) object;

	if (clock->timer_id != -1) {
		gtk_timeout_remove (clock->timer_id);
		clock->timer_id = -1;
	}

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
class_init (ClockClass *klass)
{
	GObjectClass *object_class;

	object_class = G_OBJECT_CLASS (klass);
	object_class->finalize = finalize;

	parent_class = g_type_class_peek_parent (klass);
}

static void
init (Clock *clock)
{
	clock->timer_id = -1;
	clock->seconds = 0;
}

/* API */
GType
clock_get_type (void)
{
	static GType type = 0;

	if (type == 0) {
		GTypeInfo info = {
			sizeof (ClockClass),
			NULL, NULL, (GClassInitFunc) class_init, NULL, NULL,
			sizeof (Clock), 0, (GInstanceInitFunc) init,
		};

		type = g_type_register_static (GTK_TYPE_LABEL, "Clock", &info, 0);
	}

	return type;
}

GtkWidget *
clock_new (void)
{
	Clock *clock;

	clock = g_object_new (clock_get_type(), NULL);

	return GTK_WIDGET (clock);
}

static gboolean
update_clock (gpointer data)
{
	Clock *clock = (Clock *) data;
	char str[10];
	struct tm *tm;

	clock->seconds++;
	tm = localtime (&clock->seconds);

	strftime (str, 10, "%s", tm);
	gtk_label_set (GTK_LABEL (clock), str);

	return TRUE;
}

void
clock_start (Clock *clock)
{
	if (clock->timer_id != -1) {
		return;
	}

	clock->timer_id = gtk_timeout_add (1000, update_clock, clock);
}

void
clock_stop (Clock *clock)
{
	if (clock->timer_id == -1) {
		return;
	}

	gtk_timeout_remove (clock->timer_id);
	clock->timer_id = -1;
	clock->stopped = clock->seconds;
}

void
clock_set_seconds (Clock *clock,
		   time_t seconds)
{
	clock->seconds = seconds;
}
