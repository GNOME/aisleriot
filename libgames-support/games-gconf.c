/*
 * games-gconf.h: GConf utility functions. 
 *
 * Copyright (C) 2003 Ross Burton
 *
 * Authors: Ross Burton <ross@burtonini.com>
 */

#include <glib/gstrfuncs.h>
#include <gtk/gtk.h>
#include <gconf/gconf-client.h>
#include <glib/gi18n.h> /* Yuck, this should be glib */

#include <games-gconf.h>

gchar*
games_gconf_get_string(GConfClient *client, const gchar* key, const gchar* def)
{
  GConfValue *value;
  GError *error = NULL;
  gchar *string;

  g_return_val_if_fail (client != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);

  value = gconf_client_get (client, key, &error);

  if (error != NULL) {
    g_warning (error->message);
    /* If a default was passed return that, otherwise NULL */
    return def == NULL ? NULL : g_strdup (def);
  }

  /* value is null if key was not set */
  if (value == NULL) {
    /* If a default was passed return that, otherwise NULL */
    return def == NULL ? NULL : g_strdup (def);
  }

  /* check value type */
  if (value->type != GCONF_VALUE_STRING) {
    g_warning ("Key %s is not of type string, using the default instead.", key);
    return g_strdup (def);
  }

  /* Everything looks good so far, return the string */
  string = g_strdup(gconf_value_get_string (value));
  gconf_value_free (value);
  return string;
}

gboolean
games_gconf_sanity_check_string (GConfClient *client, const gchar* key)
{
  gchar *string;
  GError *error = NULL;
  
  string = gconf_client_get_string (client, key, &error);

  if (error) {
    GtkWidget *dialog;
    dialog = gtk_message_dialog_new (NULL,
                                     0,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_OK,
                                     _("There was an error accessing GConf: %s"),
                                     error->message);
    gtk_window_set_resizable (GTK_WINDOW (dialog), FALSE);
    gtk_dialog_run(GTK_DIALOG(dialog));
    return FALSE;
  }
  if (!string) {
    GtkWidget *dialog;
    dialog = gtk_message_dialog_new (NULL,
                                     0,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_OK,
                                     "<span weight=\"bold\" size=\"larger\">%s</span>\n\n%s",
                                     _("The default configuration values could not be retrieved correctly."),
                                     _("Please check your GConf configuration, specifically that the schemas have been installed correctly."));
    gtk_label_set_use_markup (GTK_LABEL (GTK_MESSAGE_DIALOG (dialog)->label), TRUE);
    gtk_window_set_resizable (GTK_WINDOW (dialog), FALSE);
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy (dialog);
    return FALSE;
  }
  g_free (string);
  return TRUE;
}
