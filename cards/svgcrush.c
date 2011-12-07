/*
 * Copyright © 2011 Christian Persch
 *
 * This programme is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This programme is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this programme.  If not, see <http://www.gnu.org/licenses/>. 
 */

#include <glib.h>

#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>

#ifdef KEEP_TEMPFILES
#define UNLINK(s)
#else
#define UNLINK(s) unlink((s))
#endif

static char *pngcrush;

static char *
decode (const char *content,
        gsize *len)
{
  const char *comma;
  char *content_type;
  char **split;
  gboolean is_base64;
  int i;

  content += strlen ("data:");
  comma = strchr (content, ',');
  if (comma == NULL)
    return NULL;

  content_type = g_strndup (content, comma - content);
  split = g_strsplit (content_type, ";", -1);
  g_free (content_type);
  if (split == NULL)
    return NULL;
  if (strcmp (split[0], "image/png") != 0) {
    g_strfreev (split);
    return NULL;
  }

  is_base64 = FALSE;
  for (i = 1; split[i]; ++i)
    if (strcmp (split[i], "base64") == 0) {
      is_base64 = TRUE;
      break;
    }
  g_strfreev (split);

  if (!is_base64)
    return NULL;

  content = comma + 1;
  return (char *) g_base64_decode (content, len);
}

static char *
save (const char *content,
      gsize *len)
{
  char *decoded;
  char *filename;
  int fd;

  decoded = decode (content, len);
  if (decoded == NULL)
    return NULL;

  filename = g_strdup ("./svg-crush-XXXXXXXX");
  fd = g_mkstemp (filename);
  if (fd == -1) {
    g_free (filename);
    g_free (decoded);
    return NULL;
  }
  if (!g_file_set_contents (filename, decoded, *len, NULL)) {
    unlink (filename);
    g_free (filename);
    g_free (decoded);
    return NULL;
  }

  g_free (decoded);
  return filename;
}

static void
shutup (gpointer data G_GNUC_UNUSED)
{
  close (STDIN_FILENO);
  close (STDOUT_FILENO);
  close (STDERR_FILENO);
}

static char *
crush (const char *tmp_in,
       gsize *len)
{
  char *tmp_out;
  char *contents = NULL;
  int fd, status;
  gboolean retval;
  char *argv[4];

  tmp_out = g_strdup ("svg-crush-XXXXXXXX");
  fd = g_mkstemp (tmp_out);
  if (fd == -1) {
    g_free (tmp_out);
    return NULL;
  }
  close (fd);

  argv[0] = pngcrush;
  argv[1] = (char *) tmp_in;
  argv[2] = tmp_out;
  argv[3] = NULL;
  retval = g_spawn_sync (NULL /* our cwd */,
                         argv,
                         NULL /* envv */,
                         0,
                         shutup /* pngcrush is very noisy */, NULL,
                         NULL /* stdin */, NULL /* stdout */,
                         &status,
                         NULL);
  if (!retval || !WIFEXITED (status) || WEXITSTATUS (status) != 0)
    goto out;

  if (!g_file_get_contents (tmp_out, &contents, len, NULL))
    contents = NULL;

  /* pngcrush sometimes exits with status 0 but writes a zero-length file */
  if (*len == 0) {
    g_free (contents);
    unlink (tmp_out);
    link (tmp_in, tmp_out);
    g_printerr ("pngcrush choked on file %s\n", tmp_out);
    g_free (tmp_out);
    return NULL;
  }

out:
  UNLINK (tmp_out);
  g_free (tmp_out);

  return contents;
}

static char*
encode (const char *content,
        gsize len)
{
  char *encoded, *data;

  encoded = g_base64_encode ((const guchar *) content, len);
  data = g_strconcat ("data:image/png;base64,", encoded, NULL);
  g_free (encoded);

  return data;
}

static void
process (xmlNodePtr node)
{
  char *tmp_in;
  gsize in_len, out_len;
  char *crushed, *encoded;

  tmp_in = save ((const char *) node->content, &in_len);
  if (tmp_in == NULL)
    return;

  crushed = crush (tmp_in, &out_len);
  if (crushed == NULL)
    goto out;

  encoded = encode (crushed, out_len);
  xmlNodeSetContent (node, (const xmlChar *) encoded);
  g_free (encoded);

  g_print ("OUT %" G_GSIZE_FORMAT " / IN %" G_GSIZE_FORMAT " = %.1f%%\n",
           out_len, in_len, 100. * (double) out_len / (double) in_len);

out:
  UNLINK (tmp_in);
  g_free (tmp_in);
}

static gboolean
transform (xmlDocPtr doc)
{
  gboolean retval = FALSE;
  xmlXPathContextPtr ctx;
  xmlXPathObjectPtr obj;
  xmlNodeSetPtr set;
  xmlNodePtr *nodes;
  int i, n_nodes;

  ctx = xmlXPathNewContext (doc);

  obj = xmlXPathEvalExpression ((const xmlChar *) "//*/@*[namespace-uri()='http://www.w3.org/1999/xlink']", ctx);
  if (obj == NULL || obj->type != XPATH_NODESET)
    goto out;

  set = obj->nodesetval;
  nodes = set->nodeTab;
  n_nodes = set->nodeNr;

  for (i = 0; i < n_nodes; ++i) {
    xmlNodePtr node = nodes[i];
    xmlNodePtr child = node->children;

    if (strcmp ((const char *) node->name, "href") != 0)
      continue;

    if (child == NULL ||
        child->content == NULL)
      continue;

    process (child);
  }

  retval = TRUE;

out:
  if (obj)
    xmlXPathFreeObject (obj);

  xmlXPathFreeContext (ctx);

  return retval;
}

int main (int argc, char *argv[]) 
{
  int retval = 1;
  xmlDoc *doc;

  if (argc != 3) {
    g_printerr ("Usage: %s INPUT-FILE OUTPUT-FILE\n", argv[0]);
    return 1;
  }

  pngcrush = g_find_program_in_path ("pngcrush");
  if (pngcrush == NULL) {
    g_printerr ("pngcrush not found in $PATH\n");
    return 1;
  }

  doc = xmlReadFile (argv[1], "UTF-8", XML_PARSE_NONET);
  if (doc == NULL)
    goto out;

  if (!transform (doc))
    goto out;

  if (xmlSaveFile (argv[2], doc) == -1)
    goto out;

  retval = 0;

out:
  if (doc)
    xmlFreeDoc (doc);
  g_free (pngcrush);

  return retval;
}
