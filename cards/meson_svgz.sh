#!/usr/bin/env bash
# Copyright © 2019 Christian Persch
#
# This programme is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# This programme is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this programme.  If not, see <https://www.gnu.org/licenses/>.

# Really sucks that meson has no simple way to just chain a few commands
# together in a pipe to produce an output file.

set -e

xmllint="$1"
xmllint_flags="$2"
gzip="$3"
gzip_flags="$4"
infile="$5"
outfile="$6"

cat "${infile}" | "${xmllint}" ${xmllint_flags} - | "${gzip}" ${gzip_flags} > "${outfile}"
