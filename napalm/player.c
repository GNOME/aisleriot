#include <gnome.h>
#include "game.h"

typedef struct
{
    NapalmGame *game;
    NapalmPlayer *player;
    int x, y, index;
} Fire;

static void draw_tank(NapalmGame *game, gint dx, gint dy);
static void fire_weapon(GtkWidget *widget, NapalmGame *game);
static gint fire_timer(Fire *f);

void new_player(NapalmGame *game, gchar *name, gint x)
{
    NapalmPlayer *p;

    p = g_new0(NapalmPlayer, 1);

    p->name = name;
    p->health = 100;
    p->x_pos = x;
    p->y_pos = game->dirt[x];

    g_print("Creating \'%s\' at %d\n", p->name, x);

    game->players = g_list_append(game->players, p);
}

void add_players(NapalmGame *game, gchar **names, gint num)
{
    gint spacing = 640/num;
    gint x, i;

    game->cur_player = 0;
    g_print("Creating %d players\n", num);

    for(x = spacing/2, i=0; x < spacing*num; x += spacing, i++)
    {
	new_player(game, names[i], x);
    }
    fire_weapon(NULL, game);
}

void draw_players(NapalmGame *game)
{
    GList *list = game->players;
    
    while(list)
    {
	NapalmPlayer *p = list->data;

	g_print("Drawing \'%s\'\n", p->name);
	
	draw_tank(game, p->x_pos, game->dirt[p->x_pos]);

	list = list->next;
    }
}

static void draw_tank(NapalmGame *game, gint dx, gint dy)
{
    int x, y;
    
    for(x=0; x < 16; x++)
    {
	for(y=0; y < 8; y++)
	{
	    setpixel(game->buffer, x+dx, y+dy, 0xff, 0, 0);
	}
    }
}

static gint fire_timer(Fire *f)
{
    int i;
    NapalmGame *g = f->game;
    NapalmPlayer *p = f->player;

    for(i=0; i < 5; i++)
    {	
	g_print("\'%s\' is firing, index %d (%d, %d)\n", p->name, f->index, f->x, f->y);
	
	if(f->index < 200)
	{
	    f->x++;
	    f->y--;
	    f->index++;
	    setpixel(g->buffer, f->x, f->y, 0xff,0xff,0xff);
	    redraw_screen(g, f->x-1, f->y-1, 2, 2);
//	    while(gtk_events_pending()) gtk_main_iteration();
	}
	else
	{
	    g_print("\'%s\' is done firing.\n", p->name);
	    return FALSE;
	}
    }
    return TRUE;
}

static void fire_weapon(GtkWidget *widget, NapalmGame *game)
{
    Fire *f = g_new0(Fire, 1);
    GList *list;

    list = g_list_nth(game->players, 0);
    f->player = list->data;
    /* FIXME this could break simult firing ^^^^^^^^^^ */

    f->game = game;
    f->x = f->player->x_pos+8;
    f->y = f->player->y_pos;
    f->index = 0;

    gtk_timeout_add(20, (GtkFunction)fire_timer, f);
}
