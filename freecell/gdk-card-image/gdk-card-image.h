/* gdk-card-image.h -- card image loading library.  */

#ifndef __GDK_CARD_IMAGE_H
#define __GDK_CARD_IMAGE_H

#include <gdk/gdk.h>

void gdk_card_image_init (GdkWindow *window);
void gdk_card_image_unref ();

GdkPixmap* gdk_card_image_face (int suit, int value);
GdkPixmap* gdk_card_image_back ();
GdkBitmap* gdk_card_image_mask ();

/* Obsolete */
void gdk_card_image (int card_id, GdkPixmap **pixmap, GdkBitmap **mask);

#endif /* __GDK_CARD_IMAGE_H */
