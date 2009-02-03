/*
 * clock.c: 
 *
 * Copyright © 2001, 2003 Iain Holmes
 *           © 2001 Mark McLoughlin
 *
 * Authors: Iain Holmes <iain@ximian.com>
 *          Mark McLoughlin <mark@skynet.ie>
 */

#include <config.h>

#include "games-clock.h"

G_DEFINE_TYPE (GamesClock, games_clock, GTK_TYPE_LABEL)

static void
games_clock_finalize (GObject * object)
{
  GamesClock *clock_widget = GAMES_CLOCK (object);

  if (clock_widget->timer_id != 0) {
    g_source_remove (clock_widget->timer_id);
    clock_widget->timer_id = 0;
  }

  G_OBJECT_CLASS (games_clock_parent_class)->finalize (object);
}

static void
games_clock_class_init (GamesClockClass * klass)
{
  GObjectClass *object_class = (GObjectClass *) klass;

  object_class->finalize = games_clock_finalize;
}

static void
games_clock_init (GamesClock *clock_widget)
{
  clock_widget->timer_id = 0;
  clock_widget->seconds = 0;

  gtk_label_set_text (GTK_LABEL (clock_widget), "00:00:00");
}

GtkWidget *
games_clock_new (void)
{
  return g_object_new (GAMES_TYPE_CLOCK, NULL);
}

static void
clock_paint (GamesClock *clock_widget)
{
  char string[32];
  int secs;
  int mins;
  int hours;

  hours = clock_widget->seconds / 3600;
  secs = clock_widget->seconds - hours * 3600;
  mins = secs / 60;
  secs = secs - mins * 60;

  /* FIXMEchpe: i18n! */
  g_snprintf (string, sizeof (string), "%.2d:%.2d:%.2d", hours, mins, secs);

  gtk_label_set_text (GTK_LABEL (clock_widget), string);
}


static gboolean
games_clock_update (GamesClock *clock_widget)
{
  clock_widget->seconds++;

  clock_paint (clock_widget);

  return TRUE;
}

void
games_clock_start (GamesClock *clock_widget)
{
  g_return_if_fail (GAMES_IS_CLOCK (clock_widget));

  if (clock_widget->timer_id != 0)
    return;

  clock_widget->timer_id = g_timeout_add (1000,
				   (GSourceFunc) games_clock_update, clock_widget);
}

void
games_clock_stop (GamesClock *clock_widget)
{
  g_return_if_fail (GAMES_IS_CLOCK (clock_widget));

  if (clock_widget->timer_id == 0)
    return;

  g_source_remove (clock_widget->timer_id);
  clock_widget->timer_id = 0;
  clock_widget->stopped = clock_widget->seconds;
}

void
games_clock_set_seconds (GamesClock *clock_widget,
                         time_t seconds)
{
  g_return_if_fail (GAMES_IS_CLOCK (clock_widget));

  clock_widget->seconds = seconds;
  clock_paint (clock_widget);
}

time_t
games_clock_get_seconds (GamesClock *clock_widget)
{
  g_return_val_if_fail ( GAMES_IS_CLOCK (clock_widget), 0);

  return clock_widget->seconds;
}

void
games_clock_add_seconds (GamesClock *clock_widget,
                         time_t seconds)
{
  g_return_if_fail (GAMES_IS_CLOCK (clock_widget));

  clock_widget->seconds += seconds;
  clock_paint (clock_widget);
}
