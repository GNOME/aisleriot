/*
 * clock.c: 
 *
 * Copyright (C) 2001 Iain Holmes
 *           (C) 2001 Mark McLoughlin
 *
 * Authors: Iain Holmes <iain@ximian.com>
 *          Mark McLoughlin <mark@skynet.ie>
 */

#include <glib.h>

#include "games-clock.h"

static GtkLabelClass *parent_class = NULL;

static void
games_clock_finalize (GObject *object)
{
	GamesClock *clock;

	g_return_if_fail (object && GAMES_IS_CLOCK (object));

	clock = GAMES_CLOCK (object);

	if (clock->timer_id != -1) {
		g_source_remove (clock->timer_id);
		clock->timer_id = -1;
	}

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
games_clock_class_init (GamesClockClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;

	object_class->finalize = games_clock_finalize;

	parent_class = g_type_class_peek_parent (klass);
}

static void
games_clock_instance_init (GamesClock *clock)
{
	clock->timer_id = -1;
	clock->seconds = 0;

	gtk_label_set_text (GTK_LABEL (clock), "00:00:00");
}

GType
games_clock_get_type (void)
{
	static GType type = 0;

	if (!type) {
		GTypeInfo info = {
			sizeof (GamesClockClass),
			NULL,
			NULL,
			(GClassInitFunc) games_clock_class_init,
			NULL,
			NULL,
			sizeof (GamesClock),
			0,
			(GInstanceInitFunc) games_clock_instance_init,
		};

		type = g_type_register_static (GTK_TYPE_LABEL, "GamesClock", &info, 0);
	}

	return type;
}

GtkWidget *
games_clock_new (void)
{
	GamesClock *clock;

	clock = g_object_new (games_clock_get_type(), NULL);

	return GTK_WIDGET (clock);
}

static void
clock_paint (GamesClock *clock)
{
        char *string;
        int   secs;
        int   mins;
        int   hours;
        
        g_return_if_fail (clock && GAMES_IS_CLOCK (clock));
 
        secs  = clock->seconds;
        mins  = clock->seconds / 60;
        hours = clock->seconds / 3600;
        
        string = g_strdup_printf ( "%.2d:%.2d:%.2d", hours, mins, secs);
        
        gtk_label_set (GTK_LABEL (clock), string);
                                         
        g_free (string);
}


static gboolean
games_clock_update (GamesClock *clock)
{
	g_return_if_fail (clock && GAMES_IS_CLOCK (clock));

	clock->seconds++;

	clock_paint (clock);

	return TRUE;
}

void
games_clock_start (GamesClock *clock)
{
	g_return_if_fail (clock && GAMES_IS_CLOCK (clock));

	if (clock->timer_id != -1)
		return;

	clock->timer_id = g_timeout_add (1000,
					 (GSourceFunc) games_clock_update,
					 clock);
}

void
games_clock_stop (GamesClock *clock)
{
	g_return_if_fail (clock && GAMES_IS_CLOCK (clock));

	if (clock->timer_id == -1)
		return;

	g_source_remove (clock->timer_id);
	clock->timer_id = -1;
	clock->stopped = clock->seconds;
}

void
games_clock_set_seconds (GamesClock *clock,
			 time_t      seconds)
{
	g_return_if_fail (clock && GAMES_IS_CLOCK (clock));

	clock->seconds = seconds;
	clock_paint (clock);
}
