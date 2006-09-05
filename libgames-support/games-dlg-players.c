/* 
 * File: dlg_players.c
 * Author: Jason Short
 * Project: GGZ GTK Games
 * Date: 10/13/2002 (moved from GGZCards)
 * Desc: Create the "Players" Gtk dialog
 * $Id$
 *
 * Copyright (C) 2002 GGZ Development Team
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 */

#include <config.h>
#include <gnome.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <strings.h> /* For strcasecmp */

#include <gtk/gtk.h>

#include "games-dlg-players.h"

typedef enum {
	PLAYER_COLUMN_SEAT,
	PLAYER_COLUMN_TYPE,
	PLAYER_COLUMN_NAME,

	PLAYER_COLUMN_ISSPECTATOR,
	PLAYER_COLUMN_SEATNUM,

	PLAYER_COLUMNS
} PlayerColumn;

GGZMod *ggz = NULL;

typedef struct playerlists {
	GtkWidget *this;
	struct playerlists *next;
} PlayerLists;

static PlayerLists *player_lists = NULL;
static GtkWidget *dlg_players = NULL;
static int num_entries = 0;

static void update_player_list(GtkWidget * tree);
static void update_player_dialog(void);

/*static gboolean player_list_button_event(GtkWidget * widget,
					 GdkEventButton * event,
					 gpointer data);*/

void update_player_lists(void)
{
	PlayerLists *list;

	update_player_dialog();

	for (list = player_lists; list; list = list->next)
		update_player_list(list->this);
}

static void handle_ggz_seat_event(GGZMod * ggzmod, GGZModEvent e,
				  const void *data)
{
	update_player_lists();
}

void init_player_list(GGZMod * ggzmod)
{
	ggz = ggzmod;
	assert(ggz);
	ggzmod_set_handler(ggzmod, GGZMOD_EVENT_SEAT,
			   handle_ggz_seat_event);
	ggzmod_set_handler(ggzmod, GGZMOD_EVENT_SPECTATOR_SEAT,
			   handle_ggz_seat_event);
	ggzmod_set_handler(ggzmod, GGZMOD_EVENT_STATE,
			   handle_ggz_seat_event);
}

static void update_player_list(GtkWidget * tree)
{
	int p, num;
	GtkListStore *store = g_object_get_data(G_OBJECT(tree), "store");

	assert(ggz);

	gtk_list_store_clear(store);
	num_entries = 0;

	/* Put all players on the list. */
	num = ggzmod_get_num_seats(ggz);
	for (p = 0; p < num; p++) {
		GGZSeat seat = ggzmod_get_seat(ggz, p);
		GtkTreeIter iter;
		const gchar *status = NULL, *name = NULL;
		gchar num[32];

		gtk_list_store_append(store, &iter);

		snprintf(num, sizeof(num), "%d", p);
		switch (seat.type) {
		case GGZ_SEAT_PLAYER:
			status = _("Occupied");
			name = seat.name;
			break;
		case GGZ_SEAT_OPEN:
			status = _("Empty");
			name = "-";
			break;
		case GGZ_SEAT_BOT:
			status = _("Bot");
			name = seat.name;
			break;
		case GGZ_SEAT_RESERVED:
			status = _("Reserved");
			name = seat.name;
			break;
		case GGZ_SEAT_ABANDONED:
			status = _("Abandoned");
			name = seat.name;
			break;
		case GGZ_SEAT_NONE:
			status = _("-");
			name = seat.name;
			break;
		}

		gtk_list_store_set(store, &iter,
				   PLAYER_COLUMN_SEAT, num,
				   PLAYER_COLUMN_ISSPECTATOR,
				   (gboolean) FALSE, PLAYER_COLUMN_SEATNUM,
				   (gint) p, PLAYER_COLUMN_TYPE, status,
				   PLAYER_COLUMN_NAME, name, -1);

		num_entries++;
	}

	/* Append any spectators to the list */
	num = ggzmod_get_num_spectator_seats(ggz);
	for (p = 0; p < num; p++) {
		GGZSpectatorSeat seat = ggzmod_get_spectator_seat(ggz, p);
		GtkTreeIter iter;

		if (!seat.name)
			continue;

		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
				   PLAYER_COLUMN_SEAT, "-",
				   PLAYER_COLUMN_ISSPECTATOR,
				   (gboolean) TRUE, PLAYER_COLUMN_SEATNUM,
				   (gint) p, PLAYER_COLUMN_TYPE,
				   _("Spectator"), PLAYER_COLUMN_NAME,
				   seat.name, -1);
		num_entries++;
	}
}

static void update_player_dialog(void)
{
	GtkWidget *tree;

	if (!dlg_players)
		return;

	tree = g_object_get_data(G_OBJECT(dlg_players), "tree");

	update_player_list(tree);
}

static GtkWidget *create_player_list(void)
{
	GtkListStore *store;
	GtkWidget *tree;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *select;

	assert(PLAYER_COLUMNS == 5);
	store = gtk_list_store_new(PLAYER_COLUMNS,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_BOOLEAN, G_TYPE_INT);
	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	g_object_set_data(G_OBJECT(tree), "store", store);
	g_object_unref(store);

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes(_("#"), renderer,
							  "text",
							  PLAYER_COLUMN_SEAT,
							  NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes(_("Status"),
							  renderer,
							  "text",
							  PLAYER_COLUMN_TYPE,
							  NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	column =
	    gtk_tree_view_column_new_with_attributes(_("Name"), renderer,
						     "text",
						     PLAYER_COLUMN_NAME,
						     NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(select, GTK_SELECTION_SINGLE);

	/* FIXME:  This feature is not supported by gnome-games yet. 
	g_signal_connect(tree, "button-press-event",
			 GTK_SIGNAL_FUNC(player_list_button_event), NULL);
	*/
	update_player_list(tree);

	return tree;
}

GtkWidget *create_playerlist_widget(void)
{
	PlayerLists *list = ggz_malloc(sizeof(*list));
	list->this = create_player_list();
	list->next = player_lists;
	player_lists = list;

	g_signal_connect(list->this, "destroy",
			 GTK_SIGNAL_FUNC(gtk_widget_destroyed),
			 &list->this);

	return list->this;
}

static GtkWidget *create_dlg_players(GtkWindow * parent)
{
#if 0
	GtkWidget *label;
#endif
	GtkWidget *dialog, *vbox, *tree;

	/* 
	 * Create outer window.
	 */
	dialog = gtk_dialog_new_with_buttons(_("Player List"),
					     parent, 0,
					     GTK_STOCK_CLOSE,
					     GTK_RESPONSE_CLOSE, NULL);
	g_object_set_data(G_OBJECT(dialog), "dlg_players", dialog);

	/* 
	 * Get vertical box packing widget.
	 */
	vbox = GTK_DIALOG(dialog)->vbox;
	g_object_set_data(G_OBJECT(dialog), "vbox", vbox);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);
	gtk_widget_show(vbox);

#if 0
	label = gtk_label_new(_("List of players:"));
	gtk_widget_ref(label);
	gtk_widget_show(label);
	gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
#endif

	tree = create_player_list();
	gtk_widget_ref(tree);
	g_object_set_data_full(G_OBJECT(dialog), "tree", tree,
			       (GtkDestroyNotify) gtk_widget_unref);
	gtk_box_pack_start(GTK_BOX(vbox), tree, FALSE, FALSE, 0);
	gtk_widget_show(tree);

	/* 
	 * Set up callbacks
	 */
	g_signal_connect(dialog, "delete_event",
			 GTK_SIGNAL_FUNC(gtk_widget_destroy), NULL);
	g_signal_connect(dialog, "response",
			 GTK_SIGNAL_FUNC(gtk_widget_destroy), NULL);

	/* 
	 * Done!
	 */
	return dialog;
}

void create_or_raise_dlg_players(GtkWindow *window)
{
	if (dlg_players != NULL) {
		gdk_window_show(dlg_players->window);
		gdk_window_raise(dlg_players->window);
	} else {
		dlg_players = create_dlg_players(window);
		g_signal_connect(dlg_players, "destroy",
				 GTK_SIGNAL_FUNC(gtk_widget_destroyed),
				 &dlg_players);
		gtk_widget_show(dlg_players);
	}
}

static gpointer encode_seat(int spectator, int seat_num)
{
	int which = ((!!spectator) << 31) | seat_num;
	assert((seat_num & (1 << 31)) == 0);
	return GINT_TO_POINTER(which);
}

static void decode_seat(gpointer data, int *spectator, int *seat_num)
{
	int which = GPOINTER_TO_INT(data);
	*spectator = which >> 31;
	*seat_num = which & ~(1 << 31);
}

/* Get info on the player (pop up a window) */
static void player_info_activate(GtkMenuItem * menuitem, gpointer data)
{
	int spectator, seat_num;

	decode_seat(data, &spectator, &seat_num);

	/* Not implemented */
}

/* Boot the player from the table */
static void player_boot_activate(GtkMenuItem * menuitem, gpointer data)
{
	int spectator, seat_num;
	const char *name;

	decode_seat(data, &spectator, &seat_num);

	if (spectator)
		name = ggzmod_get_spectator_seat(ggz, seat_num).name;
	else
		name = ggzmod_get_seat(ggz, seat_num).name;

	assert(name);
	ggzmod_request_boot(ggz, name);
}

/* We (a spectator) will sit here. */
static void player_sit_activate(GtkMenuItem * menuitem, gpointer data)
{
	int spectator, seat_num;

	decode_seat(data, &spectator, &seat_num);

	assert(!spectator);
	ggzmod_request_sit(ggz, seat_num);
}

/* Replace the open seat with a bot */
static void player_bot_activate(GtkMenuItem * menuitem, gpointer data)
{
	int spectator, seat_num;

	decode_seat(data, &spectator, &seat_num);

	assert(!spectator);
	ggzmod_request_bot(ggz, seat_num);
}

/* Replace the bot or reserved seat with an open one */
static void player_open_activate(GtkMenuItem * menuitem, gpointer data)
{
	int spectator, seat_num;

	decode_seat(data, &spectator, &seat_num);

	assert(!spectator);
	ggzmod_request_open(ggz, seat_num);
}

void popup_player_menu(GGZSeat * seat, GGZSpectatorSeat * sseat,
		       guint button)
{
	GtkWidget *menu;
	gpointer which = encode_seat(sseat ? 1 : 0,
				     seat ? seat->num : sseat->num);
	int is_spectator, my_seat_num;
	const char *my_name;

	my_name = ggzmod_get_player(ggz, &is_spectator, &my_seat_num);

	assert((seat || sseat) && !(seat && sseat));

	menu = gtk_menu_new();

	if (sseat || seat->type == GGZ_SEAT_PLAYER) {
		GtkWidget *info;

		/* FIXME: what about bot/reservation seats? */
		info = gtk_menu_item_new_with_label(_("Info"));
		gtk_widget_ref(info);
		g_object_set_data_full(G_OBJECT(menu), "info", info,
				       (GtkDestroyNotify)
				       gtk_widget_unref);
		gtk_container_add(GTK_CONTAINER(menu), info);
		gtk_widget_set_sensitive(info, FALSE);
		g_signal_connect(info, "activate",
				 GTK_SIGNAL_FUNC(player_info_activate),
				 which);
	}

	if ((sseat && strcasecmp(sseat->name, my_name))
	    || (seat && seat->type == GGZ_SEAT_PLAYER
		&& strcasecmp(seat->name, my_name))) {
		GtkWidget *boot;

		/* FIXME: you shouldn't be able to boot yourself */
		boot = gtk_menu_item_new_with_label(_("Boot player"));
		gtk_widget_ref(boot);
		g_object_set_data_full(G_OBJECT(menu), "boot", boot,
				       (GtkDestroyNotify)
				       gtk_widget_unref);
		gtk_container_add(GTK_CONTAINER(menu), boot);
		// gtk_widget_set_sensitive(boot, FALSE);
		g_signal_connect(boot, "activate",
				 GTK_SIGNAL_FUNC(player_boot_activate),
				 which);
	}

	if (seat
	    && (seat->type == GGZ_SEAT_OPEN
		|| (seat->type == GGZ_SEAT_RESERVED
		    && !strcasecmp(my_name, seat->name)))) {
		GtkWidget *sit;
		const char *label;

		if (is_spectator)
			label = _("Sit here");
		else
			label = _("Move here");

		sit = gtk_menu_item_new_with_label(label);
		gtk_widget_ref(sit);
		g_object_set_data_full(G_OBJECT(menu), "sit", sit,
				       (GtkDestroyNotify)
				       gtk_widget_unref);
		gtk_container_add(GTK_CONTAINER(menu), sit);
		// gtk_widget_set_sensitive(sit, FALSE);
		g_signal_connect(sit, "activate",
				 GTK_SIGNAL_FUNC(player_sit_activate),
				 which);
	}

	if (seat && (seat->type == GGZ_SEAT_OPEN
		     || seat->type == GGZ_SEAT_RESERVED)) {
		GtkWidget *bot;

		bot = gtk_menu_item_new_with_label(_("Play with bot"));
		gtk_widget_ref(bot);
		g_object_set_data_full(G_OBJECT(menu), "bot", bot,
				       (GtkDestroyNotify)
				       gtk_widget_unref);
		gtk_container_add(GTK_CONTAINER(menu), bot);
		g_signal_connect(bot, "activate",
				 GTK_SIGNAL_FUNC(player_bot_activate),
				 which);
	}

	if (seat && (seat->type == GGZ_SEAT_BOT
		     || seat->type == GGZ_SEAT_RESERVED)) {
		GtkWidget *open;
		const char *label;

		if (seat->type == GGZ_SEAT_RESERVED)
			label = _("Drop reservation");
		else
			label = _("Remove bot");

		open = gtk_menu_item_new_with_label(label);
		gtk_widget_ref(open);
		g_object_set_data_full(G_OBJECT(menu), "open", open,
				       (GtkDestroyNotify)
				       gtk_widget_unref);
		gtk_container_add(GTK_CONTAINER(menu), open);
		g_signal_connect(open, "activate",
				 GTK_SIGNAL_FUNC(player_open_activate),
				 which);
	}

	gtk_widget_show_all(menu);

	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, button, 0);
}

#if 0
FIXME: Not supported by gnome-games yet.
static gboolean player_list_button_event(GtkWidget * tree,
					 GdkEventButton * buttonevent,
					 gpointer data)
{
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree));
	GtkTreePath *path = NULL;
	GtkTreeIter iter;

	if (!gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(tree),
					   buttonevent->x, buttonevent->y,
					   &path, NULL, NULL, NULL)) {
		return FALSE;
	}
	gtk_tree_model_get_iter(model, &iter, path);

	if (buttonevent->button == 3) {
		/* Right mouse button; create drop-down menu */
		gint seatnum;
		gboolean is_spectator;

		gtk_tree_model_get(model, &iter,
				   PLAYER_COLUMN_ISSPECTATOR,
				   &is_spectator, PLAYER_COLUMN_SEATNUM,
				   &seatnum, -1);

		if (is_spectator) {
			GGZSpectatorSeat sseat;
			sseat = ggzmod_get_spectator_seat(ggz, seatnum);
			popup_player_menu(NULL, &sseat,
					  buttonevent->button);
		} else {
			GGZSeat seat = ggzmod_get_seat(ggz, seatnum);
			popup_player_menu(&seat, NULL,
					  buttonevent->button);
		}
	}

	return FALSE;
}
#endif

void do_sit(void)
{
	ggzmod_request_sit(ggz, -1);
}

void do_stand(void)
{
	ggzmod_request_stand(ggz);
}
