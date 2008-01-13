/*
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


#include <config.h>

#include <glib/gi18n.h>

#include "games-card.h"

/**
 * games_card_get_name:
 * @card:
 *
 * Returns: a localised name for @card, e.g. "Face-down card" or
 * "9 of clubs", etc.
 */
const char *
games_card_get_name (Card card)
{
  return NULL;
}
