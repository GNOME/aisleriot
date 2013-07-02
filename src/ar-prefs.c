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
#include "ar-debug.h"
#include "ar-defines.h"
#include "util.h"

typedef struct {
  AisleriotWindow *window;
  GSettings *settings;
  GtkWidget *click_checkbutton;
  GtkWidget *sound_checkbutton;
  GtkWidget *animations_checkbutton;
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

/* GType impl */

G_DEFINE_TYPE (ArPrefs, ar_prefs, GTK_TYPE_DIALOG)

/* GObjectClass impl */

static void
ar_prefs_init (ArPrefs *prefs)
{
  ArPrefsPrivate *priv;

  priv = prefs->priv = G_TYPE_INSTANCE_GET_PRIVATE (prefs, AR_TYPE_PREFS, ArPrefsPrivate);

  priv->settings = g_settings_new (AR_SETTINGS_SCHEMA);

  gtk_widget_init_template (GTK_WIDGET (prefs));

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

  g_signal_connect (prefs, "response", G_CALLBACK (response_cb), NULL);
}

static void
ar_prefs_finalize (GObject *object)
{
  ArPrefs *prefs = AR_PREFS (object);
  ArPrefsPrivate *priv = prefs->priv;

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
