#include <config.h>
#include <gnome.h>
#include "gtkdial_hacked.h"
#include "game.h"

static void napalm_game_destroy(GtkWidget *widget, NapalmGame *game);
static void file_new_callback(GtkWidget *widget, gpointer data);
static void file_exit_callback(GtkWidget *widget, gpointer data);
static void help_about_callback(GtkWidget *widget, gpointer data);
static void napalm_game_new(void);
static void game_scores_callback(GtkWidget *widget, gpointer data);
static void create_fire_dialog(NapalmGame *game);
static void redraw_dirt(NapalmGame *game);
static void create_random_dirt(NapalmGame *game, gint change);

static gint drawing_area_configure_event(GtkWidget *da, GdkEventConfigure *ev, NapalmGame *game);
static gint drawing_area_expose_event(GtkWidget *widget, GdkEventExpose *ev, NapalmGame *game);

static GnomeUIInfo filemenu[] =
{
  GNOMEUIINFO_MENU_EXIT_ITEM(file_exit_callback, NULL),
  GNOMEUIINFO_END
};
  
static GnomeUIInfo helpmenu[] =
{
#if 0
  GNOMEUIINFO_HELP("Napalm Fire"),
#endif
  GNOMEUIINFO_END
};
  
GnomeUIInfo mainmenu[] = {
  GNOMEUIINFO_MENU_FILE_TREE(filemenu),
  GNOMEUIINFO_MENU_HELP_TREE(helpmenu),
  GNOMEUIINFO_END
};

static void game_scores_callback(GtkWidget *widget, gpointer data)
{
    gnome_scores_display(_("Napalm"), "Napalm", NULL, 0);
}

static void napalm_game_destroy(GtkWidget *widget, NapalmGame *game)
{
    g_free(game);
    gtk_exit(0);
}

static void napalm_game_new(void)
{
    guint i;
    gchar *names[] = {"John", "Frank", "Julie", "Tim", "Fred", "Lola", "Maria", "Matt", "David", "Rebecca"};
    GtkWidget *app;
    NapalmGame *game = g_new0(NapalmGame, 1);
    GtkWidget *vbox;

    app = gnome_app_new("Napalm_Fire", "Napalm Fire");
    game->app = app;
    gnome_app_create_menus_with_data(GNOME_APP(app), mainmenu, (gpointer)game);
    
    vbox = gtk_vbox_new(FALSE, 0);

    game->buffer = g_malloc0(640 * 480 * 3);
    for(i=0; i < (640*480*3); i+=3)
    {
    	game->buffer[i] = 0x00;
    	game->buffer[i+1] = 0x00;
    	game->buffer[i+2] = 0xaa;
    }

/* create drawing area */
    game->field = gtk_drawing_area_new();
    gtk_widget_set_usize(game->field, 640, 480);
    
    gtk_signal_connect(GTK_OBJECT(game->field), "expose_event",
		       (GtkSignalFunc)drawing_area_expose_event, game);
    gtk_signal_connect(GTK_OBJECT(game->field),"configure_event",
		       (GtkSignalFunc)drawing_area_configure_event, game);
    
    gtk_signal_connect(GTK_OBJECT(app),
		       "destroy",
		       GTK_SIGNAL_FUNC(napalm_game_destroy),
		       (gpointer)game);
    
    gtk_box_pack_start(GTK_BOX(vbox), game->field, TRUE, TRUE, 0);

    gnome_app_set_contents(GNOME_APP(app), vbox);
    gtk_widget_show_all(app);

    /* dirt */
    create_random_dirt(game, 1);
    redraw_dirt(game);

    /* players */
    add_players(game, names, 10);
    draw_players(game);

    /* firing dialog */
    create_fire_dialog(game);
}

static gint drawing_area_configure_event(GtkWidget *da, GdkEventConfigure *ev, NapalmGame *game)
{
    return TRUE;
}

static  gint drawing_area_expose_event(GtkWidget *widget, GdkEventExpose *ev, NapalmGame *game)
{
    redraw_screen(game, ev->area.x,
    			ev->area.y,
    			ev->area.width,
    			ev->area.height);
    return FALSE;
}

void redraw_screen(NapalmGame *game, gint x1, gint y1, gint width, gint height)
{
    gdk_draw_rgb_image(game->field->window,
		       game->field->style->white_gc,
		       x1, y1, width, height,
		       GDK_RGB_DITHER_NONE,
		       game->buffer + (y1 * (640*3) + x1 * 3), 640*3);
}

int main(int argc, char *argv[])
{
    gnome_score_init("napalm");
    
    bindtextdomain(PACKAGE, GNOMELOCALEDIR);
    textdomain(PACKAGE);
        
    gnome_init("Napalm Fire", "0.0", argc, argv);

    gdk_rgb_set_verbose (TRUE);
    
    gdk_rgb_init ();

    napalm_game_new();
    gtk_main();

    return 0;
}

static void file_exit_callback(GtkWidget *widget, gpointer data) 
{
    gtk_exit(0);
} 

static void create_random_dirt(NapalmGame *game, gint change)
{
    int x;

    srand(time(NULL));
    game->dirt[0] = 400;
    for(x=1; x < 640; x++)
    {   /* create a brownian (drunken walk) motion here */
	game->dirt[x] = game->dirt[x-1] + ((rand()%5)-2);
	if(game->dirt[x] < 0) game->dirt[x] = 5;
    }
}

static void redraw_dirt(NapalmGame *game)
{
    int x, y;

    for(x=0; x < 640; x++)
    {
	for(y = 479; y > game->dirt[x]; y--)
	{
	    setpixel(game->buffer, x, y, 0xf7, 0xe7, 0x8e);
	}
    }
}

static void create_fire_dialog(NapalmGame *game)
{
    GtkWidget *window, *dial1, *dial2, *button;
    GtkWidget *table;
    GtkObject *adj1, *adj2;
    GtkWidget *label1, *label2;

    adj1 = gtk_adjustment_new(90, 0, 180, 1, 10, 10);
    adj2 = gtk_adjustment_new(500, 0, 1000, 10, 100, 100);

    window = gtk_window_new(GTK_WINDOW_DIALOG);
    table = gtk_table_new(3, 2, FALSE);
    dial1 = gtk_dial_new(GTK_ADJUSTMENT(adj1));
    dial2 = gtk_dial_new(GTK_ADJUSTMENT(adj2));
    button = gtk_button_new_with_label(_("Fire!"));
    label1 = gtk_label_new(_("Direction"));
    label2 = gtk_label_new(_("Power"));

    gtk_table_attach_defaults(GTK_TABLE(table),label1, 0, 1, 0, 1);
    gtk_table_attach_defaults(GTK_TABLE(table),label2, 1, 2, 0, 1);
    gtk_table_attach_defaults(GTK_TABLE(table), dial1, 0, 1, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(table), dial2, 1, 2, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(table),button, 0, 2, 2, 3);

    gtk_container_add(GTK_CONTAINER(window), table);
    gtk_widget_show_all(window);

    gtk_object_set_data(GTK_OBJECT(window), "angle_adjust", adj1);
    gtk_object_set_data(GTK_OBJECT(window), "power_adjust", adj2);

    game->fire_dialog = window;
}
