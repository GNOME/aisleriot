/* games-setgid-io.c : Routines to perform I/O in a seperate setgid
 *                     subprocess.
 *
 * Copyright (C) 2005 by Callum McKenzie
 *
 * Time-stamp: <2006-02-15 18:55:54 callum>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* WARNING: The purpose of this code is to subvert GTK's checks
 * against setuid/setgid code. It does this by running a separate
 * process that retains setgid privileges while the main code drops
 * its privileges and runs the main code. The two processes
 * communicate by a pipe. This interface is very liberal - effectively
 * providing the standard file i/o operations with the privileges of
 * the original executable. This is bad security. The
 * privileged/non-privileged interface should be one more layer out so
 * that data passing through it can be checked more rigorously. It
 * isn't this way because a different security mechanism was assumed
 * during the design of the scoring system and this is the simplest way to 
 * retrofit the setgid security system.
 *
 * In practise security shouldn't be compromised much more than it
 * already was. The new threat is that a compromised game could
 * over-write (or create) any file with games privileges. In the old
 * code this was less likely since both the game code and the
 * privileged code would have to be compromised. In any event, if
 * games privileges are significant in your system then you have
 * bigger security problems than this code.
 */

/* We almost directly wrap the standard open/close/read/write/seek
 * functions and we wrap flock in a simpler lock primitive. 
 *
 * Each function has two forms: one half that does the actual work -
 * suffixed with _priv - and a wrapper that is called from the
 * insecure side which stuffs the arguments down the pipe connecting
 * the two processes.
 *
 * From the point of view of the caller, these functions should behave
 * transparently with the exception of having to call setgid_io_init
 * () right at the beginning of the program.
 *
 */

#include <config.h>

#include <glib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

#include "games-setgid-io.h"

enum {
  cmd_open = 1,
  cmd_close,
  cmd_read,
  cmd_write,
  cmd_seek,
  cmd_lock,
  cmd_unlock,
  cmd_stat,
  cmd_truncate
} commands;

/* Global state variables, the program only has one global instance of
   this module. */
static gboolean setgid_io_initialised = 0;
/* These names are from the point of view of the non-setgid parent. */
static int setgid_io_infd;
static int setgid_io_outfd;
static int setgid_io_child_pid;


static void
write_cmd (unsigned char cmd)
{
  int cnt;
  cnt = write (setgid_io_outfd, &cmd, 1);
  if (cnt != 1)  {
    g_warning ("An error occured while writing to file");
  }
}


static gboolean
write_n_bytes (int fd, const char *buffer, int n)
{
  int totalcnt;
  int cnt;

  totalcnt = 0;
  while (totalcnt < n) {
    cnt = write (fd, buffer + totalcnt, n);
    if (cnt == -1)
      return FALSE;
    totalcnt += cnt;
  }

  return TRUE;
}

static gboolean
read_n_bytes (int fd, char *buffer, int n)
{
  int totalcnt;
  int cnt;

  totalcnt = 0;
  while (totalcnt < n) {
    cnt = read (fd, buffer + totalcnt, n);
    if (cnt == -1)
      return FALSE;
    totalcnt += cnt;
  }

  return TRUE;
}

static void
write_int (int fd, int i)
{
  int cnt;
  cnt = write (fd, &i, sizeof (int));
  if (cnt != sizeof (int))  {
    g_warning ("An error occured while writing to file");
  }
}

static int
read_int (int fd)
{
  int out;

  if (!read_n_bytes (fd, (char *) &out, sizeof (int)))
    return 0;

  return out;
}

static void
write_off_t (int fd, off_t o)
{
  int cnt;
  cnt = write (fd, &o, sizeof (off_t));
  if (cnt != sizeof (off_t))  {
    g_warning ("An error occured while writing to file");
  }
}

static off_t
read_off_t (int fd)
{
  off_t out;

  if (!read_n_bytes (fd, (char *) &out, sizeof (off_t)))
    return 0;

  return out;
}

/* Unprivileged side. */
int
setgid_io_open (const char *path, int flags)
{
  int length;
  int fd;

  write_cmd (cmd_open);

  length = strlen (path) + 1;
  write_int (setgid_io_outfd, length);
  write_n_bytes (setgid_io_outfd, path, length);
  write_int (setgid_io_outfd, flags);

  fd = read_int (setgid_io_infd);

  return fd;
}

/* Privileged side. */
static void
setgid_io_open_priv (int outfd, int infd)
{
  int length;
  char *path;
  int flags;
  int newfd;

  length = read_int (infd);
  path = g_malloc (length);
  read_n_bytes (infd, path, length);
  flags = read_int (infd);

  newfd = open (path, flags);

  write_int (outfd, newfd);
  g_free (path);
}

/* Unprivileged side. */
int
setgid_io_close (int fd)
{
  write_cmd (cmd_close);
  write_int (setgid_io_outfd, fd);

  return read_int (setgid_io_infd);
}

/* Privileged side. */
static void
setgid_io_close_priv (int outfd, int infd)
{
  int fd;
  int result;

  fd = read_int (infd);

  result = close (fd);

  write_int (outfd, result);
}

/* Unprivileged side. */
int
setgid_io_read (int fd, char *buffer, int n)
{
  int result;

  write_cmd (cmd_read);
  write_int (setgid_io_outfd, fd);
  write_int (setgid_io_outfd, n);

  result = read_int (setgid_io_infd);

  if ((result >= 0) && (result <= n)) {
    read_n_bytes (setgid_io_infd, buffer, result);
  }

  return result;
}

/* Privileged side. */
static void
setgid_io_read_priv (int outfd, int infd)
{
  int fd;
  int n;
  int result;
  char *buffer;

  fd = read_int (infd);
  n = read_int (infd);

  buffer = g_malloc (n);
  result = read (fd, buffer, n);
  write_int (outfd, result);
  if ((result >= 0) && (result <= n)) {
    write_n_bytes (outfd, buffer, result);
  }
  g_free (buffer);
}

/* Unprivileged side. */
int
setgid_io_write (int fd, const char *buffer, int n)
{
  write_cmd (cmd_write);
  write_int (setgid_io_outfd, fd);
  write_int (setgid_io_outfd, n);
  write_n_bytes (setgid_io_outfd, buffer, n);

  return read_int (setgid_io_infd);
}

/* Privileged side. */
static void
setgid_io_write_priv (int outfd, int infd)
{
  int fd;
  int n;
  int result;
  char *buffer;

  fd = read_int (infd);
  n = read_int (infd);

  buffer = g_malloc (n);
  read_n_bytes (infd, buffer, n);

  result = write (fd, buffer, n);
  write_int (outfd, result);

  g_free (buffer);
}

/* Unprivileged side. */
off_t
setgid_io_seek (int fd, off_t offset, int whence)
{
  write_cmd (cmd_seek);
  write_int (setgid_io_outfd, fd);
  write_off_t (setgid_io_outfd, offset);
  write_int (setgid_io_outfd, whence);

  return read_off_t (setgid_io_infd);
}

/* Privileged side. */
static void
setgid_io_seek_priv (int outfd, int infd)
{
  int fd;
  off_t offset;
  int whence;
  off_t result;

  fd = read_int (infd);
  offset = read_off_t (infd);
  whence = read_int (infd);

  result = lseek (fd, offset, whence);

  write_off_t (outfd, result);
}

/* Unprivileged side. */
int
setgid_io_lock (int fd)
{
  write_cmd (cmd_lock);
  write_int (setgid_io_outfd, fd);

  return read_int (setgid_io_infd);
}

/* Privileged side. */
static void
setgid_io_lock_priv (int outfd, int infd)
{
  int fd;
  int result;
  struct flock lock;

  fd = read_int (infd);

  lock.l_type = F_WRLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = 0;
  lock.l_len = 0;

  result = fcntl (fd, F_SETLKW, &lock);

  write_int (outfd, result);
}

/* Unprivileged side. */
int
setgid_io_unlock (int fd)
{
  write_cmd (cmd_unlock);
  write_int (setgid_io_outfd, fd);

  return read_int (setgid_io_infd);
}

/* Privileged side. */
static void
setgid_io_unlock_priv (int outfd, int infd)
{
  int fd;
  int result;
  struct flock lock;

  fd = read_int (infd);

  lock.l_type = F_UNLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = 0;
  lock.l_len = 0;

  result = fcntl (fd, F_SETLKW, &lock);

  write_int (outfd, result);
}

/* Unprivileged side. */
int
setgid_io_stat (char *filename, struct stat *buffer)
{
  int length;

  write_cmd (cmd_stat);

  length = strlen (filename) + 1;
  write_int (setgid_io_outfd, length);
  write_n_bytes (setgid_io_outfd, filename, length);

  read_n_bytes (setgid_io_infd, (char *) buffer, sizeof (struct stat));
  return read_int (setgid_io_infd);
}

/* Privileged side. */
static void
setgid_io_stat_priv (int outfd, int infd)
{
  int length;
  char *filename;
  int result;
  struct stat buffer;

  length = read_int (infd);
  filename = g_malloc (length);
  read_n_bytes (infd, filename, length);

  result = stat (filename, &buffer);

  write_n_bytes (outfd, (char *) &buffer, sizeof (struct stat));
  write_int (outfd, result);
}

/* Unprivileged side. */
int
setgid_io_truncate (int fd, int length)
{
  write_cmd (cmd_truncate);
  write_int (setgid_io_outfd, fd);
  write_int (setgid_io_outfd, length);

  return read_int (setgid_io_infd);
}

/* Privileged side. */
static void
setgid_io_truncate_priv (int outfd, int infd)
{
  int fd;
  int length;
  int result;

  fd = read_int (infd);
  length = read_int (infd);
  result = ftruncate (fd, length);

  write_int (outfd, result);
}

G_GNUC_NORETURN
static void
setgid_io_pipe_watcher (int outfd, int infd)
{
  fd_set watchfds;
  char command;
  int cnt;

  FD_ZERO (&watchfds);
  FD_SET (infd, &watchfds);

  while (1) {
    select (infd + 1, &watchfds, NULL, NULL, NULL);
    cnt = read (infd, &command, 1);
    if (cnt == 1) {
      switch (command) {
      case cmd_open:
	setgid_io_open_priv (outfd, infd);
	break;
      case cmd_close:
	setgid_io_close_priv (outfd, infd);
	break;
      case cmd_read:
	setgid_io_read_priv (outfd, infd);
	break;
      case cmd_write:
	setgid_io_write_priv (outfd, infd);
	break;
      case cmd_seek:
	setgid_io_seek_priv (outfd, infd);
	break;
      case cmd_lock:
	setgid_io_lock_priv (outfd, infd);
	break;
      case cmd_unlock:
	setgid_io_unlock_priv (outfd, infd);
	break;
      case cmd_stat:
	setgid_io_stat_priv (outfd, infd);
	break;
      case cmd_truncate:
	setgid_io_truncate_priv (outfd, infd);
	break;
      default:
	g_warning ("Invalid command to setgid_io: ignored.\n");
      }
    } else {
      exit (0);
    }
  }
}

void
setgid_io_init (void)
{
  gid_t safegid;
  int setgid_io_inpipe[2];
  int setgid_io_outpipe[2];

  g_return_if_fail (setgid_io_initialised == 0);
  if (pipe (setgid_io_inpipe) != 0){
    g_warning("Unable to create pipe");
  }
  if (pipe (setgid_io_outpipe) != 0){
    g_warning("Unable to create pipe");
  }

  if ((setgid_io_child_pid = fork ()) != 0) {
    close (setgid_io_inpipe[1]);
    close (setgid_io_outpipe[0]);

    setgid_io_infd = setgid_io_inpipe[0];
    setgid_io_outfd = setgid_io_outpipe[1];

    safegid = getgid ();
    setregid (safegid, safegid);
  } else {
    close (setgid_io_inpipe[0]);
    close (setgid_io_outpipe[1]);
    close (STDIN_FILENO);

    setgid_io_pipe_watcher (setgid_io_inpipe[1], setgid_io_outpipe[0]);
    /* We should never, ever, reach here. */
    g_assert_not_reached ();
  }

  setgid_io_initialised = 1;
}
