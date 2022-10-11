/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Juergen Reuter <reuter@ipd.uka.de>

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

#ifndef CUSTOS_HH
#define CUSTOS_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

struct Custos
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));

private:
  static void add_streepjes (Grob *me, int pos, int interspaces,
                             Stencil *custos_);
  static Stencil create_ledger_line (Interval x_extent, Grob *me);
};

#endif // CUSTOS_HH
