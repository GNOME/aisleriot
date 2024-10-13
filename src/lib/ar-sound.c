/*
 * games-sound.c: common sound player for gnome-games 
 *
 * Copyright © 2007-2008 Andreas Røsdal
 * Copyright © 2009 Christian Persch
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>

#ifdef ENABLE_SOUND
#include <canberra-gtk.h>
#endif

#include "ar-debug.h"
#include "ar-runtime.h"

#include "ar-sound.h"

#ifdef ENABLE_SOUND

static gboolean sound_enabled = FALSE;

typedef enum {
  GAMES_SOUND_PLAIN,
  GAMES_SOUND_FOR_EVENT,
  GAMES_SOUND_FOR_WIDGET
} GamesSoundCanberraType;

static void
ar_sound_canberra_play (const char *sound_name,
                           GamesSoundCanberraType type,
                           gpointer data)
{
  char *name, *path;
#ifdef GNOME_ENABLE_DEBUG
  int rv = 0;
#endif

  if (!sound_enabled)
    return;

  name = g_strdup_printf ("%s.ogg", sound_name);
  path = ar_runtime_get_file (AR_RUNTIME_SOUND_DIRECTORY, name);
  g_free (name);

  switch (type) {
    case GAMES_SOUND_PLAIN:
#if 1
      /* FIXMEchpe: instead, make sure all games call ar_sound_init()
       * themselves!
       */
      ar_sound_init (data);
#endif

#ifdef GNOME_ENABLE_DEBUG
      _AR_DEBUG_IF (AR_DEBUG_SOUND) {
        if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (data), "games-sound-initialised")))
          ar_debug_print (AR_DEBUG_SOUND,
                              "ar_sound_play_for_screen called for display %s screen %d but canberra-gtk context not initialised!",
                              gdk_display_get_name (gdk_screen_get_display (data)),
                              gdk_screen_get_number (data));
      }
#endif /* GNOME_ENABLE_DEBUG */

#ifdef GNOME_ENABLE_DEBUG
      rv =
#endif
        ca_context_play (
                         ca_gtk_context_get_for_screen (data),
                         0,
                         CA_PROP_MEDIA_NAME, sound_name,
                         CA_PROP_MEDIA_FILENAME, path,
                         NULL);
      break;
    case GAMES_SOUND_FOR_EVENT:
#ifdef GNOME_ENABLE_DEBUG
      rv =
#endif
        ca_gtk_play_for_event (data,
                               0,
                               CA_PROP_MEDIA_NAME, sound_name,
                               CA_PROP_MEDIA_FILENAME, path,
                               NULL);
      break;
    case GAMES_SOUND_FOR_WIDGET:
#ifdef GNOME_ENABLE_DEBUG
      rv =
#endif
        ca_gtk_play_for_widget (data,
                                0,
                                CA_PROP_MEDIA_NAME, sound_name,
                                CA_PROP_MEDIA_FILENAME, path,
                                NULL);
      break;
    default:
      g_assert_not_reached ();
  }

  ar_debug_print (AR_DEBUG_SOUND,
                      "libcanberra playing sound %s [file %s]: %s\n",
                      sound_name, path, ca_strerror (rv));

  g_free (path);
}

#endif /* ENABLE_SOUND */

/**
 * ar_sound_init:
 * @screen: a #GdkScreen
 *
 * Initialises the sound context of @screen. Should be called
 * when creating a window on @screen, or when a window is moved to
 * @screen. It is safe to call this multiple times for the same
 * @screen.
 */
void
ar_sound_init (GdkScreen *screen)
{
#ifdef ENABLE_SOUND
  ca_context *context;

  if (screen == NULL)
    screen = gdk_screen_get_default ();

  if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (screen), "games-sound-initialised")))
    return;

  ar_debug_print (AR_DEBUG_SOUND,
                      "Initialising canberra-gtk context for display %s screen %d\n",
                      gdk_display_get_name (gdk_screen_get_display (screen)),
                      gdk_screen_get_number (screen));

  context = ca_gtk_context_get_for_screen (screen);
  if (!context)
    return;

  ca_context_change_props (context,
                           CA_PROP_MEDIA_ROLE, "game",
                           NULL);

  g_object_set_data (G_OBJECT (screen), "games-sound-initialised", GINT_TO_POINTER (TRUE));
#endif /* ENABLE_SOUND */
}

/**
 * ar_sound_play:
 * @sound_name: the sound file to player
 * 
 * Plays a sound with the given filename from the
 * AR_RUNTIME_SOUND_DIRECTORY directory in .ogg format.
 * Sound is played asynchronously; i.e. this function does not block
 * until the sound has finished playing.
 *
 * Use ar_sound_play_for_screen(), ar_sound_play_for_event()
 * or ar_sound_play_for_widget() instead.
 */
void
ar_sound_play (const gchar * sound_name)
{
  ar_sound_play_for_screen (sound_name, gdk_screen_get_default ());
}
    
/**
 * ar_sound_play_for_screen:
 * @sound_name: the sound file to player
 * 
 * Plays a sound with the given filename from the
 * AR_RUNTIME_SOUND_DIRECTORY directory in .ogg format.
 * Sound is played asynchronously; i.e. this function does not block
 * until the sound has finished playing.
 *
 * Consider using ar_sound_play_for_event() or ar_sound_play_for_widget()
 * instead.
 */
void
ar_sound_play_for_screen (const gchar * sound_name,
                             GdkScreen *screen)
{
#if defined(ENABLE_SOUND)
  ar_sound_canberra_play (sound_name, GAMES_SOUND_PLAIN, screen);
#endif
}

/**
 * ar_sound_play_for_event:
 * @sound_name: the name of the sound to play
 * @event: the #GdkEvent associated with the sound
 *
 * Plays a sound for @event.
 * See ar_sound_play() for more information.
 */
void
ar_sound_play_for_event (const gchar *sound_name,
                            GdkEvent *event)
{
#ifdef ENABLE_SOUND
  ar_sound_canberra_play (sound_name, GAMES_SOUND_FOR_EVENT, event);
#endif /* ENABLE_SOUND */
}

/**
 * ar_sound_play_for_widget:
 * @sound_name: the name of the sound to play
 * @widget: the #GtkWidget to play the sound for
 *
 * Plays a sound for @widget. Use ar_sound_play_for_event() instead
 * if the sound is associated with an event.
 * 
 * See ar_sound_play() for more information.
 */
void
ar_sound_play_for_widget (const gchar *sound_name,
                             GtkWidget *widget)
{
#ifdef ENABLE_SOUND
  ar_sound_canberra_play (sound_name, GAMES_SOUND_FOR_WIDGET, widget);
#endif /* ENABLE_SOUND */
}

/**
 * ar_sound_enable:
 * @enabled:
 *
 * Enables or disables sound support.
 */
void
ar_sound_enable (gboolean enabled)
{
#ifdef ENABLE_SOUND
  sound_enabled = enabled;
#endif /* ENABLE_SOUND */
}

/**
 * ar_sound_is_enabled:
 *
 * Returns: %TRUE iff sound support is enabled.
 */
gboolean
ar_sound_is_enabled (void)
{
#ifdef ENABLE_SOUND
  return sound_enabled;
#else
  return FALSE;
#endif /* ENABLE_SOUND */
}

/**
 * ar_sound_is_available:
 *
 * Returns: whether sound is available
 */
gboolean
ar_sound_is_available (void)
{
#ifdef ENABLE_SOUND
  return TRUE;
#else
  return FALSE;
#endif /* ENABLE_SOUND */
}
