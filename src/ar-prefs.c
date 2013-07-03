/*  
 * Copyright © 2009, 2013 Christian Persch <chpe@gnome.org>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include "ar-prefs.h"

#include <gio/gio.h>
#include <glib/gi18n.h>

#include "ar-application.h"
#include "ar-card-themes.h"
#include "ar-debug.h"
#include "ar-defines.h"
#include "util.h"

typedef struct {
  AisleriotWindow *window;
  GSettings *settings;
  GtkWidget *click_checkbutton;
  GtkWidget *sound_checkbutton;
  GtkWidget *animations_checkbutton;
  GtkWidget *theme_combobox;
  GtkListStore *theme_store;
} ArPrefsPrivate;

struct _ArPrefs
{
  GtkDialog parent;

  /*< private >*/
  ArPrefsPrivate *priv;
};

struct _ArPrefsClass
{
  GtkDialogClass parent_class;
};

enum {
  PROP_0,
  PROP_WINDOW
};

enum {
  COL_DISPLAY_NAME,
  COL_THEME_INFO
};

/* private functions */

static void
response_cb (GtkWidget *dialog,
             int response,
             gpointer user_data)
{
  if (response == GTK_RESPONSE_HELP) {
    aisleriot_show_help (dialog, NULL);
    return;
  }

  gtk_widget_destroy (dialog);
}

static void
theme_combobox_changed_cb (GtkComboBox *combo,
                           ArPrefs *prefs);

static void
theme_changed_cb (GSettings *settings,
                  const char *key,
                  ArPrefs *prefs)
{
  ArPrefsPrivate *priv = prefs->priv;
  GtkTreeModel *model = GTK_TREE_MODEL (priv->theme_store);
  GtkTreeIter iter;
  gboolean found = FALSE;
  ArCardThemeInfo *info;
  const char *theme;

  g_settings_get (settings, key, "&s", &theme);

  if (!gtk_tree_model_get_iter_first (model, &iter))
    goto done;

  do {
    gtk_tree_model_get (model, &iter, COL_THEME_INFO, &info, -1);

    found = g_str_equal (ar_card_theme_info_get_persistent_name (info), theme);

    ar_card_theme_info_unref (info);
    if (found)
      break;
  } while (gtk_tree_model_iter_next (model, &iter));

 done:

  g_signal_handlers_block_by_func (priv->theme_combobox, G_CALLBACK (theme_combobox_changed_cb), prefs);

  if (found)
    gtk_combo_box_set_active_iter (GTK_COMBO_BOX (priv->theme_combobox), &iter);
  else
    gtk_combo_box_set_active_iter (GTK_COMBO_BOX (priv->theme_combobox), NULL);

  g_signal_handlers_unblock_by_func (priv->theme_combobox, G_CALLBACK (theme_combobox_changed_cb), prefs);
}

static void
theme_combobox_changed_cb (GtkComboBox *combo,
                           ArPrefs *prefs)
{
  ArPrefsPrivate *priv = prefs->priv;
  GtkTreeIter iter;
  ArCardThemeInfo *info;
  
  if (!gtk_combo_box_get_active_iter (combo, &iter))
    return;

  gtk_tree_model_get (GTK_TREE_MODEL (priv->theme_store), &iter,
                      COL_THEME_INFO, &info,
                      -1);

  g_signal_handlers_block_by_func (priv->settings, G_CALLBACK (theme_changed_cb), prefs);
  g_settings_set_string (priv->settings, AR_SETTINGS_CARD_THEME_KEY,
                         ar_card_theme_info_get_persistent_name (info));
  g_signal_handlers_unblock_by_func (priv->settings, G_CALLBACK (theme_changed_cb), prefs);

  ar_card_theme_info_unref (info);
}

/* GType impl */

G_DEFINE_TYPE (ArPrefs, ar_prefs, GTK_TYPE_DIALOG)

/* GObjectClass impl */

static void
ar_prefs_init (ArPrefs *prefs)
{
  ArApplication *application = AR_APP;
  ArPrefsPrivate *priv;
  GList *list, *l;
  GtkListStore *store;
  ArCardThemes *card_themes;

  priv = prefs->priv = G_TYPE_INSTANCE_GET_PRIVATE (prefs, AR_TYPE_PREFS, ArPrefsPrivate);

  priv->settings = g_settings_new (AR_SETTINGS_SCHEMA);

  gtk_widget_init_template (GTK_WIDGET (prefs));

  /* Populate the card theme combo */
  store = priv->theme_store = gtk_list_store_new (2, G_TYPE_STRING, AR_TYPE_CARD_THEME_INFO);

  card_themes = ar_application_get_card_themes (application);
  ar_card_themes_request_themes (card_themes);
  list = ar_card_themes_get_themes (card_themes);
  for (l = list; l != NULL; l = l->next) {
    ArCardThemeInfo *info = (ArCardThemeInfo *) l->data;
    GtkTreeIter iter;

    gtk_list_store_insert_with_values (store, &iter, -1,
                                       COL_DISPLAY_NAME,
                                       ar_card_theme_info_get_display_name (info),
                                       COL_THEME_INFO,
                                       info,
                                       -1);
  }
  g_list_free_full (list, (GDestroyNotify) ar_card_theme_info_unref);

  gtk_combo_box_set_model (GTK_COMBO_BOX (priv->theme_combobox), GTK_TREE_MODEL (store));
  g_object_unref (store);

  /* Bind settings */
  g_settings_bind (priv->settings, AR_SETTINGS_CLICK_TO_MOVE_KEY,
                   priv->click_checkbutton, "active",
                   G_SETTINGS_BIND_DEFAULT);
  g_settings_bind (priv->settings, AR_SETTINGS_ENABLE_SOUND_KEY,
                   priv->sound_checkbutton, "active",
                   G_SETTINGS_BIND_DEFAULT);
#ifdef HAVE_CLUTTER
  g_settings_bind (priv->settings, AR_SETTINGS_ENABLE_ANIMATIONS_KEY,
                   priv->animations_checkbutton, "active",
                   G_SETTINGS_BIND_DEFAULT);
#else
  gtk_widget_hide (priv->animations_checkbutton);
#endif

  theme_changed_cb (priv->settings, AR_SETTINGS_CARD_THEME_KEY, prefs);
  g_signal_connect (priv->settings, "changed::" AR_SETTINGS_CARD_THEME_KEY,
                    G_CALLBACK (theme_changed_cb), prefs);
  g_signal_connect (priv->theme_combobox, "changed",
                    G_CALLBACK (theme_combobox_changed_cb), prefs);

  g_signal_connect (prefs, "response", G_CALLBACK (response_cb), NULL);
}

static void
ar_prefs_finalize (GObject *object)
{
  ArPrefs *prefs = AR_PREFS (object);
  ArPrefsPrivate *priv = prefs->priv;

  g_signal_handlers_disconnect_by_func (priv->settings, G_CALLBACK (theme_changed_cb), prefs);

  g_clear_object (&priv->settings);

  G_OBJECT_CLASS (ar_prefs_parent_class)->finalize (object);
}

static void
ar_prefs_set_property (GObject      *object,
                       guint         property_id,
                       const GValue *value,
                       GParamSpec   *pspec)
{
  ArPrefs *prefs = AR_PREFS (object);
  ArPrefsPrivate *priv = prefs->priv;
  
  switch (property_id) {
  case PROP_WINDOW:
    priv->window = g_value_get_object (value);
    break;
    
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
ar_prefs_class_init (ArPrefsClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  g_type_class_add_private (klass, sizeof (ArPrefsPrivate));

  object_class->finalize = ar_prefs_finalize;
  object_class->set_property = ar_prefs_set_property;

  /**
   * ArPrefs:window:
   *
   * The parent #AisleriotWindow
   */
  g_object_class_install_property
    (object_class,
     PROP_WINDOW,
     g_param_spec_object ("window", NULL, NULL,
                          AISLERIOT_TYPE_WINDOW,
                          G_PARAM_WRITABLE |
                          G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS));

  gtk_widget_class_set_template_from_resource (widget_class, "/org/gnome/aisleriot/ui/prefs.ui");
  gtk_widget_class_bind_child (widget_class, ArPrefsPrivate, click_checkbutton);
  gtk_widget_class_bind_child (widget_class, ArPrefsPrivate, sound_checkbutton);
  gtk_widget_class_bind_child (widget_class, ArPrefsPrivate, animations_checkbutton);
  gtk_widget_class_bind_child (widget_class, ArPrefsPrivate, theme_combobox);
}

/* public API */

/**
 * ar_prefs_new:
 * @window: the parent #AisleriotWindow
 *
 * Return value: a new #ArPrefs
 */
GtkWidget *
ar_prefs_new (AisleriotWindow *window)
{
  return g_object_new (AR_TYPE_PREFS,
                       "type", GTK_WINDOW_TOPLEVEL,
                       "transient-for", window,
                       "destroy-with-parent", TRUE,
                       "window", window,
                       NULL);
}
