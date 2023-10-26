#!/usr/bin/env perl

#  Aisleriot Game Name Internationalizator
#
#  Copyright (C) 2001 Free Software Foundation.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Authors:
#  2006 - Richard Hoelscher <rah@rahga.com>
#  2001 - Kenneth Christiansen <kenneth@gnu.org>

open OUTFILE, "> game-names.h";

opendir(DIR, "../games/");
@dir = sort(readdir(DIR));
closedir(DIR);

print OUTFILE "/* This is a generated file; DO NOT EDIT */\n";

foreach $_ (@dir) {
  next if (/^(api|card-monkey)\.scm$/);
  if (s/^(.)(.*).scm/\u$1$2/) { # Match scm filenames. Upcase first letter.
    s/-(.)/ \u$1/g;             # Replace underscores and following letter
				# with space and upcase letter.
    print OUTFILE "/* Translators: this string is the name of a game of patience.\n";
    print OUTFILE " If there is an established standard name for this game in your\n";
    print OUTFILE " locale, use that; otherwise you can translate this string\n";
    print OUTFILE " freely, literally, or not at all, at your option.\n";
    print OUTFILE " */\n";
    print OUTFILE "N_(\"$_\")\n\n";
  }
}
