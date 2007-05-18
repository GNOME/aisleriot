/*
 * games-sound.c: common sound player for gnome-games 
 *
 * Copyright (C) 2007 Andreas Røsdal
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

#include "config.h"

#include <gnome.h>
#include <glib.h>
#include <gst/gst.h>

#include "games-sound.h"

static GMainLoop *loop;
static GstElement *pipeline;
static GstBus *bus;
static gboolean sound_enabled = TRUE;
static gboolean sound_init = FALSE;

/* Gstreamer callback function. */
static gboolean
bus_watch (GstBus * bus, GstMessage * message, gpointer data)
{
  GMainLoop *loop = (GMainLoop *) data;

  switch (GST_MESSAGE_TYPE (message)) {
  case GST_MESSAGE_ERROR:{
      GError *err;
      gchar *debug;

      gst_message_parse_error (message, &err, &debug);
      g_print (_("Error playing sound: %s\n"), err->message);
      g_error_free (err);
      g_free (debug);

      g_main_loop_quit (loop);
      break;
    }
  case GST_MESSAGE_EOS:
    g_main_loop_quit (loop);
    break;
  default:
    break;
  }

  return TRUE;
}

/* Initializes the games-sound support, GStreamer and threads. */
static void
games_sound_init (void)
{
  sound_init = TRUE;
  gst_init (NULL, NULL);
  loop = g_main_loop_new (NULL, FALSE);

  pipeline = gst_element_factory_make ("playbin", "playbin");

  bus = gst_pipeline_get_bus (GST_PIPELINE (pipeline));
  gst_bus_add_watch (bus, bus_watch, loop);
  gst_object_unref (bus);

  if (!g_thread_supported ()) {
    g_thread_init (NULL);
    gdk_threads_init ();
  } 
}

/* This function is called as a separate thread, playing the sound. */
static void 
games_sound_thread_run (void *filename) 
{
  gchar *fullname = NULL;

  if (pipeline != NULL && loop != NULL) {
    fullname = g_strdup_printf ("file:///%s/%s.ogg", SOUNDDIR, (char *) filename);
    g_object_set (G_OBJECT (pipeline), "uri", fullname, NULL);
    gst_element_set_state (pipeline, GST_STATE_PLAYING);
    g_main_loop_run (loop);
    gst_element_set_state (pipeline, GST_STATE_NULL);
    g_free (fullname);
  }

}

/* Plays a sound with the given filename using GStreamer. The sound file is stored in 
 * the SOUNDDIR directory in .ogg format. Sound is played in a separate thread.
 */
void
games_sound_play (const gchar * filename)
{
  GThread *sound_thread;
  GError *err = NULL;

  if (!sound_enabled)
    return;
  if (!sound_init)
    games_sound_init ();

  if ((sound_thread = g_thread_create ((GThreadFunc)games_sound_thread_run, (void *)filename, TRUE, &err)) == NULL) {
     g_print (_("Error playing sound: %s\n"), err->message);
     g_error_free ( err ) ;
  }
 
}

/* Enables or disables sound support. */
void
games_sound_enable (gboolean status)
{
  sound_enabled = status;
}

/* Determines if sound support is enabled. */
gboolean
games_sound_is_enabled (void)
{
  return sound_enabled;
}
