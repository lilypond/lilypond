/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef PERCENT_REPEAT_ITEM_HH
#define PERCENT_REPEAT_ITEM_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Percent_repeat_item_interface
{
public:
  DECLARE_GROB_INTERFACE();
  DECLARE_SCHEME_CALLBACK (beat_slash, (SCM));
  DECLARE_SCHEME_CALLBACK (double_percent, (SCM));
  static Stencil x_percent (Grob *, int);
  static Stencil brew_slash (Grob *);
};

#endif /* PERCENT_REPEAT_ITEM_HH */

