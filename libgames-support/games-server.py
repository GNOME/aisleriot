#!/usr/bin/python

import select
import socket
import errno
import traceback
import fcntl
import os
import sys
import threading
from sys import stdin, stdout, stderr
from threading import Thread

class Client:
    WAITING = 1
    PARTNERED = 2
    DEAD = 3

    def __repr__(self):
        return "<Client instance at 0x%s: %s / %s / %s / %s>" % (hex(id(self)),
                                                            self.sock.fileno(), self.state, self.evmask, len(self.outbuf))

    def __init__(self, server, sock):
        self.outbuf = ""
        self.sock = sock
        self.server = server
        self.partner = None
        self.state = self.WAITING
        self.evmask = 0

    def destroy(self):
        self.state = self.DEAD
        partner = self.partner
        self.partner = None
        if partner:
            self.server.del_client(partner)
        self.sock.close()

    def set_partner(self, partner):
        self.partner = partner
        self.state = self.PARTNERED
        self.register()

    def register(self):
        evmask = select.POLLHUP|select.POLLNVAL|select.POLLERR
        if self.state == self.PARTNERED:
            evmask = evmask | select.POLLIN
        if len(self.outbuf):
            evmask = evmask | select.POLLOUT
        if self.evmask != evmask:
            self.server.poller.register(self.sock.fileno(), evmask)
            self.evmask = evmask

    def write_val(self, val):
        self.outbuf += val
        self.register()

    def handle_write(self):
        try:
            n = self.sock.send(self.outbuf)
        except (IOError, OSError, socket.error):
            return self.server.del_client(self)
        if n <= 0:
            return self.server.del_client(self)
        self.outbuf = self.outbuf[n:]
        self.register()

    def handle_read(self):
        assert self.state == self.PARTNERED
        try:
            data = self.sock.recv(8192)
        except (IOError, OSError, socket.error):
            return self.server.del_client(self)
        if not len(data):
            return self.server.del_client(self)
        self.partner.write_val(data)

class Server(Thread):
    def __init__(self, game_port):
	Thread.__init__(self)
        self.poller = select.poll()
        self.asock = socket.socket(socket.AF_INET, socket.SOCK_STREAM, socket.IPPROTO_TCP)
        self.asock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.asock.bind(('', game_port))
        self.asock.listen(10)
	print "Game service running on port" , game_port
	
        self.cnxlist = {}
        self.waitlist = []
        self.ccount = 0

        self.poller.register(self.asock.fileno(), select.POLLIN)

    def run(self):
        while 1:
            try:
                evs = self.poller.poll()
            except select.error, e:
                if e[0] == errno.EINTR:
                    continue
            for I in evs:
                if I[0] == self.asock.fileno():
                    csock, addr = self.asock.accept()
                    fcntl.fcntl(csock.fileno(), fcntl.F_SETFL, os.O_NONBLOCK)
                    print "\nNew connection from %s, waitlist has %d items" % (addr, len(self.waitlist))
                    cli = Client(self, csock)
                    self.cnxlist[csock.fileno()] = cli
                    self.waitlist.append(cli)
                    self.ccount += 1
                    cli.register()
                else:
                    if not self.cnxlist.has_key(I[0]):
                        assert 0, "Maybe something is broken with FD %s" % I[0]
                        continue
                    cnx = self.cnxlist[I[0]]
                    if (I[1] & (select.POLLHUP|select.POLLNVAL|select.POLLERR)):
                        self.del_client(cnx)
                    else:
                        if (I[1] & select.POLLIN):
                            cnx.handle_read()
                        if cnx.state != cnx.DEAD and (I[1] & select.POLLOUT):
                            cnx.handle_write()
            while len(self.waitlist) >= 2:
                c1 = self.waitlist[0]
                c2 = self.waitlist[1]
                self.waitlist = self.waitlist[2:]
                c1.write_val("set_peer 1\n") # First in line gets to move first :)
                c2.write_val("set_peer 31\n")
                c1.set_partner(c2)
                c2.set_partner(c1)

    def del_client(self, cli):
        if cli.state == cli.DEAD:
            return

        del self.cnxlist[cli.sock.fileno()]
        self.poller.unregister(cli.sock.fileno())
        cli.destroy()
        if cli in self.waitlist:
            self.waitlist.remove(cli)
        self.ccount -= 1

# This is minimal code to make the process a daemon in the Unix
# environment. See W. R. Stevens "Advanced Programming in the Unix
# Environment". See chapter 13, esp. section 13.3

pid = os.fork ()
if (pid != 0):  
    os._exit (0)
os.setsid ()
os.chdir ("/")

for fd in range (0, os.sysconf ("SC_OPEN_MAX")):
    try:
        os.close (fd)
    except:
        pass

open ("/dev/null", "r")   # new stdin
open ("/dev/null", "rw")  # new stdout
open ("/dev/null", "rw")  # new stderr

# Create the servers - one for each game
iagno1 = Server(26478)
gnibbles1 = Server(26479)

# Start the servers
iagno1.start()
gnibbles1.start()


