
#ifndef _NAPALM_GAME_H_
#define _NAPALM_GAME_H_

#include <gnome.h>

typedef struct _NapalmGame NapalmGame;
typedef struct _NapalmPlayer NapalmPlayer;
typedef struct _NapalmWeapon NapalmWeapon;
typedef struct _NapalmWeaponDef NapalmWeaponDef;

struct _NapalmGame
{
    GtkWidget *app; /* GnomeApp */
    GtkWidget *fire_dialog;

    GList *players;
    GtkWidget *field; /* this is drawing area */

    gint dirt[640];
    guchar *buffer;
   
    guint round;
    guint cur_player;
};

struct _NapalmPlayer
{
    GList *weapons; /* list of Weapons */
    GList *addons; /* list of Addons */
    
    gchar *name;
    guint health;
    gint x_pos, y_pos;
    guint tank_type;
    gint score;
};

struct _NapalmWeapon
{
    guint count; /* number of shots left */
    NapalmWeaponDef *weapon; /* thsi points to the weapon definition */
};

struct _NapalmWeaponDef
{
    gchar *name;
    guint cost; /* cost in credits */
    
    guint (*fire)(NapalmGame *, NapalmPlayer *);
};

#define setpixel(buf, x, y, r, g, b) buf[((y)*3)*(640)+((x)*3)]   = (r); \
                                     buf[((y)*3)*(640)+((x)*3)+1] = (g); \
                                     buf[((y)*3)*(640)+((x)*3)+2] = (b);

void redraw_screen(NapalmGame *game, gint, gint, gint, gint);
void add_players(NapalmGame *game, gchar **names, gint num);
void draw_players(NapalmGame *game);

#endif
