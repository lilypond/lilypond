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

#ifndef METER_HH
#define METER_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

/**
   Print a time_signature sign.

   TODO:

   2+3+2/8 time_signatures
*/
struct Time_signature
{
  DECLARE_GROB_INTERFACE();
  static Stencil special_time_signature (Grob *, SCM, int, int);
  static Stencil numbered_time_signature (Grob *, int, int);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};
#endif // METER_HH

