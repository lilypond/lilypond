/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2010 Han-Wen Nienhuys

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

#ifndef BAR_HH
#define BAR_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

/**
   A vertical bar.
*/
class Bar
{
public:
  DECLARE_GROB_INTERFACE();

  static Stencil compound_barline (Grob *, string, Real height);
  static Stencil simple_barline (Grob *, Real wid, Real height);
  DECLARE_SCHEME_CALLBACK (get_staff_bar_size, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
};
#endif // BAR_HH

