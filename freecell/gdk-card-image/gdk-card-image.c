/* gdk-cardpixmap.c

 */


#include <gdk_imlib.h>

static char *suitnames[] = {"Clubs", "Spades", "Hearts", "Diamonds"};

static char *ranknames[] = {"Ace", "Deuce", "Three", "Four", "Five", "Six",
"Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King"};

GdkImlibImage *
gdk_card_image (int cardid)
{
  static char buf[512];
  sprintf (buf, "%s/%s.%s.xpm",
	   CARDPIXMAPDIR,
	   ranknames[cardid % 13], suitnames[(cardid / 13) % 4]);
  return gdk_imlib_load_image (buf);
}
