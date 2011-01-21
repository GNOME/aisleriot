/* games-setgid-io.h : Routines to perform I/O in a seperate setgid
 *                     subprocess.
 *
 * Copyright (C) 2005 by Callum McKenzie
 *
 * Time-stamp: <2005-12-18 12:17:26 callum>
 */


#ifndef GAMES_SETGID_IO_H
#define GAMES_SETGID_IO_H

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

void  setgid_io_init     (void);
int   setgid_io_open     (const char *path,
                          int flags);
int   setgid_io_close    (int fd);
int   setgid_io_read     (int fd,
                          char *buffer,
                          int n);
int   setgid_io_write    (int fd,
                          const char *buffer,
                          int n);
off_t setgid_io_seek     (int fd,
                          off_t offset,
                          int whence);
int   setgid_io_lock     (int fd);
int   setgid_io_unlock   (int fd);
int   setgid_io_stat     (char *filename,
                          struct stat *buffer);
int   setgid_io_truncate (int fd,
                          int length);

#endif /* GAMES_SETGID_IO_H */
