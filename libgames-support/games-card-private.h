/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008 Christian Persch

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */


#ifndef GAMES_CARD_PRIVATE_H
#define GAMES_CARD_PRIVATE_H

G_BEGIN_DECLS

#define GAMES_CARD_ID(suit, rank) ((13*(suit)) + ((rank-1)%13))

guint _games_card_to_index (Card card);

G_END_DECLS

#endif /* !GAMES_CARD_PRIVATE_H */
