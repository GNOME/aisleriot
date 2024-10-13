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
#include "ar-clock.h"


struct _ArClock {
  GtkLabel parent_instance;

  guint update_timeout_id;
  gboolean update;
  gboolean started;
  time_t start_time;
  time_t stop_time;
};


G_DEFINE_TYPE (ArClock, ar_clock, GTK_TYPE_LABEL);


static void
clock_paint (ArClock *clock_widget)
{
  char string[32];
  time_t seconds;
  int secs;
  int mins;
  int hours;

  seconds = ar_clock_get_seconds (clock_widget);
  hours = seconds / 3600;
  secs = seconds - hours * 3600;
  mins = secs / 60;
  secs = secs - mins * 60;

  /* FIXMEchpe: i18n! */
  g_snprintf (string, sizeof (string), "%.2d:%.2d:%.2d", hours, mins, secs);

  gtk_label_set_text (GTK_LABEL (clock_widget), string);
}

static gboolean
ar_clock_update (ArClock *clock_widget)
{
  clock_paint (clock_widget);

  return TRUE;
}

static void
ar_clock_start_timer (ArClock *clock_widget)
{
  if (clock_widget->update_timeout_id != 0)
    return;

  clock_widget->update_timeout_id =
    gdk_threads_add_timeout_seconds (1, (GSourceFunc) ar_clock_update, clock_widget);
}

static void
ar_clock_stop_timer (ArClock *clock_widget)
{
  if (clock_widget->update_timeout_id != 0) {
    g_source_remove (clock_widget->update_timeout_id);
    clock_widget->update_timeout_id = 0;
  }
}

static void
ar_clock_finalize (GObject * object)
{
  ArClock *clock_widget = AR_CLOCK (object);

  ar_clock_stop_timer (clock_widget);

  G_OBJECT_CLASS (ar_clock_parent_class)->finalize (object);
}

static void
ar_clock_class_init (ArClockClass * klass)
{
  GObjectClass *object_class = (GObjectClass *) klass;

  object_class->finalize = ar_clock_finalize;
}

static void
ar_clock_init (ArClock *clock_widget)
{
  clock_widget->update_timeout_id = 0;
  clock_widget->start_time = clock_widget->stop_time = 0;
  clock_widget->started = FALSE;
  clock_widget->update = TRUE;

  /* FIXMEchpe: call clock_paint() instead */
  gtk_label_set_text (GTK_LABEL (clock_widget), "00:00:00");
}

/**
 * ar_clock_new:
 * 
 * Create a new game clock
 * 
 * Returns: A new #ArClock
 **/
GtkWidget *
ar_clock_new (void)
{
  return g_object_new (AR_TYPE_CLOCK, NULL);
}

/**
 * ar_clock_start:
 * @clock_widget:
 *
 * Records the current time as the start time, and starts
 * updating the clock if updates are enabled (see
 * ar_clock_set_update()).
 */
void
ar_clock_start (ArClock *clock_widget)
{
  g_return_if_fail (AR_IS_CLOCK (clock_widget));

  if (clock_widget->started)
    return; /* nothing to do */

  clock_widget->started = TRUE;
  clock_widget->start_time = time (NULL) - (clock_widget->stop_time - clock_widget->start_time);

  if (clock_widget->update)
    ar_clock_start_timer (clock_widget);
}

/**
 * ar_clock_is_started:
 * @clock_widget:
 *
 * Returns: whether @clock_widget is running
 */
gboolean
ar_clock_is_started   (ArClock *clock_widget)
{
  g_return_val_if_fail (AR_IS_CLOCK (clock_widget), FALSE);

  return clock_widget->started;
}

/**
 * ar_clock_stop:
 * @clock_widget:
 *
 * Records the current time as the stop time, and stops
 * updating the clock.
 */
void
ar_clock_stop (ArClock *clock_widget)
{
  g_return_if_fail (AR_IS_CLOCK (clock_widget));

  if (!clock_widget->started)
    return;

  clock_widget->started = FALSE;
  clock_widget->stop_time = time (NULL);

  ar_clock_stop_timer (clock_widget);
  clock_paint (clock_widget);
}

/**
 * ar_clock_reset:
 * @clock_widget:
 *
 * Resets the time in @clock_widget to 0.
 */
void
ar_clock_reset (ArClock *clock_widget)
{
  g_return_if_fail (AR_IS_CLOCK (clock_widget));

  clock_widget->start_time = clock_widget->stop_time = time (NULL);

  clock_paint (clock_widget);
}

/**
 * ar_clock_get_seconds:
 * @clock_widget:
 *
 * Returns: the elapsed time in @clock_widget
 */
time_t
ar_clock_get_seconds (ArClock *clock_widget)
{
  g_return_val_if_fail (AR_IS_CLOCK (clock_widget), 0);

  if (clock_widget->started)
    return time (NULL) - clock_widget->start_time;
  else
    return clock_widget->stop_time - clock_widget->start_time;
}

/**
 * ar_clock_add_seconds:
 * @clock_widget:
 * @seconds:
 *
 * Adds @seconds to the reported elapsed time in @clock_widget.
 */
void
ar_clock_add_seconds (ArClock *clock_widget,
                      time_t   seconds)
{
  g_return_if_fail (AR_IS_CLOCK (clock_widget));

  if (!clock_widget->started) {
    g_warning ("Clock not started, cannot add seconds!\n");
    return;
  }

  clock_widget->start_time -= seconds;
  clock_paint (clock_widget);
}

/**
 * ar_clock_set_update:
 * @clock_widget:
 *
 * Sets whether the clock will automatically update itself every second
 * to display the currently elapsed time.
 *
 * Use this e.g. to disable updates while the clock is invisible.
 */
void
ar_clock_set_update (ArClock  *clock_widget,
                     gboolean  do_update)
{
  g_return_if_fail (AR_IS_CLOCK (clock_widget));

  do_update = do_update != FALSE;
  if (do_update == clock_widget->update)
    return;

  clock_widget->update = do_update;
  if (do_update) {
    ar_clock_start_timer (clock_widget);
    clock_paint (clock_widget);
  } else {
    ar_clock_stop_timer (clock_widget);
  }
}
