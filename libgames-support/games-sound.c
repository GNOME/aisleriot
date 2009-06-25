/*
 * games-sound.c: common sound player for gnome-games 
 *
 * Copyright © 2007-2008 Andreas Røsdal
 * Copyright © 2009 Christian Persch
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

#if defined(HAVE_CANBERRA_GTK)
#include <canberra-gtk.h>
#elif defined(HAVE_SDL_MIXER)
#include "SDL.h"
#include "SDL_mixer.h"
#endif

#include "games-debug.h"
#include "games-runtime.h"

#include "games-sound.h"

#ifdef ENABLE_SOUND

static gboolean sound_enabled = FALSE;

#ifdef HAVE_SDL_MIXER

static gboolean sound_init = FALSE;

static void
games_sound_sdl_play (const gchar *filename)
{
  Mix_Chunk *wave = NULL;
  gchar *name, *path;

  name = g_strdup_printf ("%s.ogg", filename);
  path = games_runtime_get_file (GAMES_RUNTIME_SOUND_DIRECTORY, name);
  g_free (name);

  wave = Mix_LoadWAV (path);
  if (wave == NULL) {
    _games_debug_print (GAMES_DEBUG_SOUND,
                        "Error playing sound %s: %s\n", path, Mix_GetError ());
  }

  Mix_PlayChannel (-1, wave, 0);
  g_free (path);
}

#endif /* HAVE_SDL_MIXER */

#ifdef HAVE_CANBERRA_GTK

#ifdef CA_CHECK_VERSION
#if CA_CHECK_VERSION (0, 13)
#define HAVE_CANBERRA_GTK_MULTIHEAD_SAFE
#endif
#endif

typedef enum {
  GAMES_SOUND_PLAIN,
  GAMES_SOUND_FOR_EVENT,
  GAMES_SOUND_FOR_WIDGET
} GamesSoundCanberraType;

static void
games_sound_canberra_play (const char *sound_name,
                           GamesSoundCanberraType type,
                           gpointer data)
{
  char *name, *path;
  int rv;

  if (!sound_enabled)
    return;

  name = g_strdup_printf ("%s.ogg", sound_name);
  path = games_runtime_get_file (GAMES_RUNTIME_SOUND_DIRECTORY, name);
  g_free (name);

  switch (type) {
    case GAMES_SOUND_PLAIN:
#if 1
      /* FIXMEchpe: instead, make sure all games call games_sound_init()
       * themselves!
       */
      games_sound_init (data);
#endif

#ifdef GNOME_ENABLE_DEBUG
      _GAMES_DEBUG_IF (GAMES_DEBUG_SOUND) {
        if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (data), "games-sound-initialised")))
          _games_debug_print (GAMES_DEBUG_SOUND,
                              "games_sound_play_for_screen called for display %s screen %d but canberra-gtk context not initialised!",
                              gdk_display_get_name (gdk_screen_get_display (data)),
                              gdk_screen_get_number (data));
      }
#endif /* GNOME_ENABLE_DEBUG */

      rv =  ca_context_play (
#ifdef HAVE_CANBERRA_GTK_MULTIHEAD_SAFE
                             ca_gtk_context_get_for_screen (data),
#else
                             ca_gtk_context_get (),
#endif /* HAVE_CANBERRA_GTK_MULTIHEAD_SAFE */
                             0,
                             CA_PROP_MEDIA_NAME, sound_name,
                             CA_PROP_MEDIA_FILENAME, path,
                             NULL);
      break;
    case GAMES_SOUND_FOR_EVENT:
      rv =  ca_gtk_play_for_event (data,
                                   0,
                                   CA_PROP_MEDIA_NAME, sound_name,
                                   CA_PROP_MEDIA_FILENAME, path,
                                   NULL);
      break;
    case GAMES_SOUND_FOR_WIDGET:
      rv =  ca_gtk_play_for_widget (data,
                                    0,
                                    CA_PROP_MEDIA_NAME, sound_name,
                                    CA_PROP_MEDIA_FILENAME, path,
                                    NULL);
      break;
    default:
      g_assert_not_reached ();
  }

  _games_debug_print (GAMES_DEBUG_SOUND,
                      "libcanberra playing sound %s [file %s]: %s\n",
                      sound_name, path, ca_strerror (rv));

  g_free (path);
}

#endif /* HAVE_CANBERRA_GTK */

#ifdef HAVE_SDL_MIXER

/* Initializes the games-sound support */
static void
games_sound_init (void)
{
  const int audio_rate = MIX_DEFAULT_FREQUENCY;
  const int audio_format = MIX_DEFAULT_FORMAT;
  const int audio_channels = 2;

/* Sounds don't sound good on Windows unless the buffer size is 4k,
 * but this seems to cause strange behaviour on other systems,
 * such as a delay before playing the sound. */
#ifdef G_OS_WIN32
  const size_t buf_size = 4096;
#else
  const size_t buf_size = 1024;
#endif

  SDL_Init (SDL_INIT_AUDIO | SDL_INIT_NOPARACHUTE);

  if (Mix_OpenAudio (audio_rate, audio_format, audio_channels, buf_size) < 0) {
    _games_debug_print (GAMES_DEBUG_SOUND,
                        "Error calling Mix_OpenAudio\n");
    return;
  }
}

#endif /* HAVE_SDL_MIXER */

#endif /* ENABLE_SOUND */

/**
 * games_sound_init:
 * @screen: a #GdkScreen
 *
 * Initialises the sound context of @screen. Should be called
 * when creating a window on @screen, or when a window is moved to
 * @screen. It is safe to call this multiple times for the same
 * @screen.
 */
void
games_sound_init (GdkScreen *screen)
{
#ifdef ENABLE_SOUND
#ifdef HAVE_CANBERRA_GTK
  ca_context *context;

  if (screen == NULL)
    screen = gdk_screen_get_default ();

  if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (screen), "games-sound-initialised")))
    return;

  _games_debug_print (GAMES_DEBUG_SOUND,
                      "Initialising canberra-gtk context for display %s screen %d\n",
                      gdk_display_get_name (gdk_screen_get_display (screen)),
                      gdk_screen_get_number (screen));

#ifdef HAVE_CANBERRA_GTK_MULTIHEAD_SAFE
  context = ca_gtk_context_get_for_screen (screen);
#else
  context = ca_gtk_context_get ();
#endif /* HAVE_CANBERRA_GTK_MULTIHEAD_SAFE */

  if (!context)
    return;

  ca_context_change_props (context,
                           CA_PROP_MEDIA_ROLE, "game",
                           NULL);

  g_object_set_data (G_OBJECT (screen), "games-sound-initialised", GINT_TO_POINTER (TRUE));
#endif /* HAVE_CANBERRA_GTK */
#endif /* ENABLE_SOUND */
}

/**
 * games_sound_play:
 * @filename: the sound file to player
 * 
 * Plays a sound with the given filename from the
 * GAMES_RUNTIME_SOUND_DIRECTORY directory in .ogg format.
 * Sound is played asynchronously; i.e. this function does not block
 * until the sound has finished playing.
 *
 * Use games_sound_play_for_screen(), games_sound_play_for_event()
 * or games_sound_play_for_widget() instead.
 */
void
games_sound_play (const gchar * sound_name)
{
  games_sound_play_for_screen (sound_name, gdk_screen_get_default ());
}
    
/**
 * games_sound_play_for_screen:
 * @filename: the sound file to player
 * 
 * Plays a sound with the given filename from the
 * GAMES_RUNTIME_SOUND_DIRECTORY directory in .ogg format.
 * Sound is played asynchronously; i.e. this function does not block
 * until the sound has finished playing.
 *
 * Consider using games_sound_play_for_event() or games_sound_play_for_widget()
 * instead.
 */
void
games_sound_play_for_screen (const gchar * sound_name,
                             GdkScreen *screen)
{
#if defined(HAVE_CANBERRA_GTK)
  games_sound_canberra_play (sound_name, GAMES_SOUND_PLAIN, screen);

#elif defined(HAVE_SDL_MIXER)

  if (!sound_enabled)
    return;
  if (!sound_init)
    games_sound_init ();

  games_sound_sdl_play (sound_name);
#endif
}

/**
 * games_sound_play_for_event:
 * @sound_name: the name of the sound to play
 * @event: the #GdkEvent associated with the sound
 *
 * Plays a sound for @event.
 * See games_sound_play() for more information.
 */
void
games_sound_play_for_event (const gchar *sound_name,
                            GdkEvent *event)
{
#ifdef ENABLE_SOUND
#ifdef HAVE_CANBERRA_GTK
  games_sound_canberra_play (sound_name, GAMES_SOUND_FOR_EVENT, event);
#else
  games_sound_play (sound_name);
#endif /* HAVE_CANBERRA_GTK */
#endif /* ENABLE_SOUND */
}

/**
 * games_sound_play_for_widget:
 * @sound_name: the name of the sound to play
 * @event: the #GdkEvent associated with the sound
 *
 * Plays a sound for @widget. Use games_sound_play_for_event() instead
 * if the sound is associated with an event.
 * 
 * See games_sound_play() for more information.
 */
void
games_sound_play_for_widget (const gchar *sound_name,
                             GtkWidget *widget)
{
#ifdef ENABLE_SOUND
#ifdef HAVE_CANBERRA_GTK
  games_sound_canberra_play (sound_name, GAMES_SOUND_FOR_WIDGET, widget);
#else
  games_sound_play (sound_name);
#endif /* HAVE_CANBERRA_GTK */
#endif /* ENABLE_SOUND */
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
#ifdef ENABLE_SOUND
  sound_enabled = enabled;
#endif /* ENABLE_SOUND */
}

/**
 * games_sound_is_enabled:
 *
 * Returns: %TRUE iff sound support is enabled.
 */
gboolean
games_sound_is_enabled (void)
{
#ifdef ENABLE_SOUND
  return sound_enabled;
#else
  return FALSE;
#endif /* ENABLE_SOUND */
}

/**
 * games_sound_is_available:
 *
 * Returns: whether sound is available
 */
gboolean
games_sound_is_available (void)
{
#ifdef ENABLE_SOUND
  return TRUE;
#else
  return FALSE;
#endif /* ENABLE_SOUND */
}
