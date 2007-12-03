/*
 * games-sound.c: common sound player for gnome-games 
 *
 * Copyright © 2007 Andreas Røsdal
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 */

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>

#ifdef HAVE_GSTREAMER
#include <gst/gst.h>
#endif /* HAVE_GSTREAMER */

#include "games-sound.h"

#ifdef HAVE_GSTREAMER

static GstElement *pipeline;
static gboolean sound_enabled = FALSE;
static gboolean sound_init = FALSE;
static GThreadPool *threads;

/* This function is called as a separate thread, playing the sound. */
static void
games_sound_thread_run (gchar * data, gchar * user_data)
{
  gchar *fullname = NULL;

  gboolean done = FALSE;
  GstBus *bus;

  bus = gst_pipeline_get_bus (GST_PIPELINE (pipeline));
  /* Set URL for sound to play. */
  fullname = g_strdup_printf ("file:///%s/%s.ogg", SOUNDDIR, (char *) data);
  g_object_set (G_OBJECT (pipeline), "uri", fullname, NULL);
  /* Set playbin to playing state. */
  gst_element_set_state (pipeline, GST_STATE_PLAYING);

  do {
    GstMessage *message;

    /* wait for message on the bus */
    message = gst_bus_timed_pop (bus, GST_CLOCK_TIME_NONE);

    switch (GST_MESSAGE_TYPE (message)) {
    case GST_MESSAGE_EOS:
      done = TRUE;
      break;
    case GST_MESSAGE_ERROR:{
	GError *err;
	gchar *debug;

	gst_message_parse_error (message, &err, &debug);
	g_print (_("Error playing sound: %s\n"), err->message);
	g_error_free (err);
	g_free (debug);

	done = TRUE;
	break;
      }
    default:
      break;
    }
    gst_message_unref (message);
  }
  while (!done);

  gst_element_set_state (pipeline, GST_STATE_NULL);
  g_free (fullname);
}

/* Initializes the games-sound support, GStreamer and threads. */
static void
games_sound_init (void)
{
  GError *err = NULL;

  g_assert (g_thread_supported ());

  pipeline = gst_element_factory_make ("playbin", "playbin");

  threads = g_thread_pool_new ((GFunc) games_sound_thread_run,
			       NULL, 10, FALSE, &err);
  sound_init = TRUE;

}

#endif /* HAVE_GSTREAMER */

/**
 * games_sound_add_option_group:
 * @context: a #GOptionContext
 *
 * Adds the GStreamer option group to @context.
 */
void
games_sound_add_option_group (GOptionContext *context)
{
#ifdef HAVE_GSTREAMER
  g_option_context_add_group (context, gst_init_get_option_group ());
#endif /* HAVE_GSTREAMER */
}

/**
 * games_sound_play:
 * @filename: the sound file to player
 * 
 * Plays a sound with the given filename using GStreamer. The sound file is stored in
 * the SOUNDDIR directory in .ogg format. Sound is played in a separate thread.
 */
void
games_sound_play (const gchar * filename)
{
#ifdef HAVE_GSTREAMER
  GError *err = NULL;

  if (!sound_enabled)
    return;
  if (!sound_init)
    games_sound_init ();

  g_thread_pool_push (threads, (gchar *) filename, &err);
#endif /* HAVE_GSTREAMER */
}

/**
 * games_sound_enable:
 * @enabled:
 *
 * Enables or disables sound support.
 */
void
games_sound_enable (gboolean enabled)
{
#ifdef HAVE_GSTREAMER
  sound_enabled = enabled;
#endif /* HAVE_GSTREAMER */
}

/**
 * games_sound_is_enabled:
 *
 * Returns: %TRUE iff sound support is enabled.
 */
gboolean
games_sound_is_enabled (void)
{
#ifdef HAVE_GSTREAMER
  return sound_enabled;
#else
  return FALSE;
#endif /* HAVE_GSTREAMER */
}

/**
 * games_sound_is_available:
 *
 * Returns: whether sound is available
 */
gboolean
games_sound_is_available (void)
{
#ifdef HAVE_GSTREAMER
  return TRUE;
#else
  return FALSE;
#endif /* HAVE_GSTREAMER */
}
