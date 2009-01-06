/*
   Copyright © 2004 Callum McKenzie
   Copyright © 2007, 2008 Christian Persch

   This library is free software; you can redistribute it and'or modify
   it under the terms of the GNU Library General Public License as published 
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */


#ifndef GAMES_CARD_PRIVATE_H
#define GAMES_CARD_PRIVATE_H

G_BEGIN_DECLS

#define GAMES_CARD_ID(suit, rank) ((13*(suit)) + ((rank-1)%13))

guint _games_card_to_index (Card card);

G_END_DECLS

#endif /* !GAMES_CARD_PRIVATE_H */
