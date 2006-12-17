#!/usr/bin/env python
#
# Copyright (c) 2005 by Callum McKenzie
#
# A script to find the last update to the NEWS file and then extract 
# every ChangeLog entry since then. Useful for constructing the next
# NEWS entry.
#
# Version 1.0: Created. 20050725 crm.
#

import os
import re
from sys import stdout

# This is up here so it only gets executed once.
datere = re.compile ("^(\d\d\d\d)-(\d\d)-(\d\d)")
newsre = re.compile ("NEWS:")

def getfilelist ():
    """Find every ChangeLog file under the current directory and
    return the paths as a list"""
    changelogs = []
    for root, dirs, files in os.walk ("."):
        for name in files:
            if name == "ChangeLog":
                changelogs.append (os.path.join (root, name))
    return changelogs

def getdate (line):
    """Find a date at the beginning of a string and return a triplet
    with the year, month and date. Return None if a date can't be
    parsed out of it."""
    m = datere.match (line)
    if m:
        return map (int, m.group (1, 2, 3))
    else:
        return None

def comparedates (date1, date2):
    for i in range (0,3):
        if (date1[i] < date2[i]):
            return 1
        if (date1[i] > date2[i]):
            return -1
    return 0
    
def readuntil (file, target):
    """Read a file and print it to stdout. Stop when a line starting with
    the target date is read. target is a year, month, day triplet."""
    # Print a header
    n = (80 - 3 - len(file))/2
    print "\n" + "-"*n, file, "-"*n + "\n"
    # Now scan the until the required date is reached
    lines = open (file)
    for line in lines:
        date = getdate (line)
        if date != None:
            if comparedates (target, date) <= 0:
                return
        stdout.write (line)

def findlastdate ():
    """Scans the toplevel ChangeLog and finds the last date where we
    updated the NEWS file with a ChangeLog entry. Then returns the
    date."""
    for line in open ("ChangeLog"):
        newdate = getdate (line)
        if newdate != None:
            date = newdate
        if newsre.search (line):
            return date
    return None

target = findlastdate ()

for f in getfilelist ():
    readuntil (f, findlastdate ())


