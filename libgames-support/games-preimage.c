/* games-preimage.c
   Copyright 2004 Richard Hoelscher
   
   This library is free software; you can redistribute it and'or modify
   it under the terms of the GNU Library General Public License as published 
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Authors:   Richard Hoelscher <rah@rahga.com> */

/* Cache raster and vector images and render them to a specific size. */

#include "config.h"

#include <glib.h>
#include <glib/gi18n-lib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <libgnomevfs/gnome-vfs-utils.h>

#include "games-preimage.h"

#define LOAD_BUFFER_SIZE 4096

G_DEFINE_TYPE (GamesPreimage, games_preimage, G_TYPE_OBJECT);

static void 
games_preimage_init (GamesPreimage *preimage) 
{
  preimage->scalable  = FALSE;
  preimage->srcsize   = 0;
  preimage->width     = 0;
  preimage->height    = 0;
  preimage->pixbuf    = NULL;
  preimage->srcbuffer = NULL;
}

static void
games_preimage_finalize (GamesPreimage *preimage)
{
  g_free (preimage->srcbuffer);
  if (preimage->pixbuf != NULL) g_object_unref (preimage->pixbuf);
}

static void 
games_preimage_class_init (GamesPreimageClass *klass)
{
  GObjectClass *oclass = G_OBJECT_CLASS (klass);
  oclass->finalize = (GObjectFinalizeFunc) games_preimage_finalize;
}

GamesPreimage * 
games_preimage_new (void)
{
  return GAMES_PREIMAGE (g_object_new (GAMES_TYPE_PREIMAGE, NULL));
}

/* size_info_cb:
 * Handles the size-prepared signal in games_preimage_new_from_file */
static void
size_info_cb (GdkPixbufLoader *loader, 
	      gint              width,
	      gint              height,
	      gpointer         data)
{
  GdkPixbufFormat *format;

  struct {
    gint width;
    gint height;
    gboolean scalable;
  } *info = data;

  g_return_if_fail (width > 0 && height > 0);
  
  format = gdk_pixbuf_loader_get_format (loader);
  
  info->scalable = gdk_pixbuf_format_is_scalable(format);
 
  /* SVG files should provide a default width and height, which can be 
   * used to describe the aspect ratio. If this is not specified, the
   * the loader generates a warning, then provides an arbitrary width and
   * height. (currently 512x512) */
  info->width  = width;
  info->height = height;
}

/* size_prepared_cb:
 * Handles the size-prepared signal in games_preimage_render */
static void
size_prepared_cb (GdkPixbufLoader *loader,
		  gint              width,   
		  gint              height,
		  gpointer         data)
{
  struct {
    gint width;
    gint height;
  } *info = data;
  
  width  = info->width;
  height = info->height;
  gdk_pixbuf_loader_set_size (loader, width, height);
}                                                                              

/* games_preimage_render:
 * Create a GdkPixbuf from a GamesPreimage at the specified
 * width and height. */
GdkPixbuf *
games_preimage_render (GamesPreimage * preimage,
                       gint width, 
		       gint height, 
		       GError **error)
{
  GdkPixbuf * pixbuf;
  
  g_return_val_if_fail (width > 0 && height > 0, NULL);
  g_return_val_if_fail (preimage != NULL, NULL);
  
  if (preimage->scalable){   /* Render vector image */
    
    GdkPixbufLoader *loader;

    struct {
      gint width;
      gint height;
    } info;
    
    gchar *buffer      = preimage->srcbuffer;
    gsize  buffer_size = preimage->srcsize;
    
    info.width  = width;
    info.height = height;

    loader = gdk_pixbuf_loader_new ();
    
    g_signal_connect (loader, "size-prepared", 
		      G_CALLBACK (size_prepared_cb), &info);
    
    if (!gdk_pixbuf_loader_write (loader, buffer, buffer_size, error)){
      gdk_pixbuf_loader_close (loader, NULL);
      g_object_unref (loader);
      return NULL;
    }

    if (!gdk_pixbuf_loader_close (loader, error)){        	
      g_object_unref (loader);
      return NULL;
    }
    
    pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);
    
    if (!pixbuf) {
      g_object_unref (loader);
      g_set_error (error,
		   GDK_PIXBUF_ERROR,
		   GDK_PIXBUF_ERROR_FAILED,
		   _("Image rendering failed."));
      return NULL;
    }
    
    g_object_ref (pixbuf);
    g_object_unref (loader);
    
  } else {                   /* Render raster image */

    pixbuf = gdk_pixbuf_scale_simple (preimage->pixbuf,
				      width, height,
				      GDK_INTERP_BILINEAR);
  } 
  
  return pixbuf;
}

/* games_preimage_new_from_uri:
 * Take a filename and use it to create a GamesPreimage, which can
 * be used to render a GdkPixbuf later. */
GamesPreimage *
games_preimage_new_from_uri (const gchar *uri, 
			      GError **error)
{
  GamesPreimage   *preimage;
  GdkPixbufLoader *loader;
  GdkPixbuf       *pixbuf;
  
  gchar *buffer;
  gsize  buffer_size;
  
  gint   length; 
  gint   offset = 0;
  
  struct {
    gint      width;
    gint      height;
    gboolean scalable;
  } info;
  
  info.scalable = FALSE;
  
  g_return_val_if_fail (uri != NULL, NULL);
  
  if (gnome_vfs_read_entire_file (uri, &buffer_size, &buffer) != GNOME_VFS_OK){
    return NULL;
  }
  
  loader = gdk_pixbuf_loader_new ();
  g_signal_connect (loader, "size-prepared", G_CALLBACK (size_info_cb), &info);
  
  /* write to the loader, breaking early if we find a vector image*/
  while ( (buffer_size>offset) && !(info.scalable) ){
    length=MIN(buffer_size-offset, LOAD_BUFFER_SIZE);
    if (!gdk_pixbuf_loader_write (loader, buffer+offset, length, error)) {
      gdk_pixbuf_loader_close  (loader, NULL);
      g_object_unref (loader);
      g_free (buffer);
      return NULL;
    }
    offset += length;
  }
  
  if (info.scalable) {   /* Prepare a vector image... */
    
    gdk_pixbuf_loader_close (loader, NULL);
    g_object_unref (loader);
    
    preimage = games_preimage_new();
    
    preimage->scalable  = info.scalable;
    preimage->width     = info.width;
    preimage->height    = info.height;
    preimage->srcbuffer = buffer;
    preimage->srcsize   = buffer_size;
    
  } else {              /* ...Or prepare a raster image */
   
    g_free(buffer);

    if (!gdk_pixbuf_loader_close (loader, error)) {
      g_object_unref (loader);
      return NULL;
    }
    
    pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);
    
    if (!pixbuf) {
      g_object_unref (loader);
      g_set_error (error,
		   GDK_PIXBUF_ERROR,
		   GDK_PIXBUF_ERROR_FAILED,
		   _("Image rendering failed."));
      return NULL;
    }
    
    g_object_ref (pixbuf);
    g_object_unref (loader);
    
    preimage = games_preimage_new();
    
    preimage->scalable = info.scalable;
    preimage->pixbuf   = pixbuf;
    preimage->width    = info.width;
    preimage->height   = info.height;
  }
  
  return preimage;	
}

gboolean
games_preimage_is_scalable (GamesPreimage * preimage)
{
  g_return_val_if_fail (preimage != NULL, FALSE);
  return preimage->scalable;
}

gint
games_preimage_get_width (GamesPreimage * preimage)
{
  g_return_val_if_fail (preimage != NULL, 0);
  return preimage->width;
}

gint
games_preimage_get_height (GamesPreimage * preimage)
{
  g_return_val_if_fail (preimage != NULL, 0);
  return preimage->height;
}

GdkPixbuf *
games_preimage_render_unscaled_pixbuf (GamesPreimage * preimage)
{
  GdkPixbuf * unscaled_pixbuf;
  
  g_return_val_if_fail (preimage != NULL, NULL);
  
  if ((unscaled_pixbuf = preimage->pixbuf)){
    g_object_ref (unscaled_pixbuf);
  } else {
    unscaled_pixbuf = games_preimage_render (preimage, preimage->width,
                                             preimage->height, NULL);
  }
  
  return unscaled_pixbuf;
}
