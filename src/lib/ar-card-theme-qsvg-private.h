/*
  Copyright © 2004 Callum McKenzie
  Copyright © 2007, 2008, 2010, 2014 Christian Persch

  This programme is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This programme is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this programme.  If not, see <http://www.gnu.org/licenses/>. */

/* Authors:   Callum McKenzie <callum@physics.otago.ac.nz> */

#include <QApplication>
#include <QtSvg/QtSvg>

#define MAX_N_BACKS (10)

struct _ArCardThemeQSvgClass {
  ArCardThemeClass parent_class;

  QApplication *app;
};

struct _ArCardThemeQSvg {
  ArCardTheme parent_instance;

  QSvgRenderer *renderer;
  QSvgRenderer *slot_renderer;

  QRectF back_rect;
  QSize card_size;

  char *backs[MAX_N_BACKS];
  guint n_backs : 4; /* enough bits for MAX_N_BACKS */
  guint back_index : 4; /* same */
  guint has_2_jokers : 1;
  guint has_joker : 1;
  guint legacy : 1;
};
