/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef FONT_INTERFACE_HH
#define FONT_INTERFACE_HH

#include "font-metric.hh"
#include "grob-interface.hh"

struct Font_interface
{
  static SCM text_font_alist_chain (Grob *);
  static SCM music_font_alist_chain (Grob *);
  static Font_metric *get_default_font (Grob *);
};

#endif /* FONT_INTERFACE_HH */
