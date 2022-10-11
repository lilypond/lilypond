/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef STAFF_SYMBOL_HH
#define STAFF_SYMBOL_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

#include <vector>

/**
   TODO: add linethickness as parameter.
*/
class Staff_symbol
{
public:
  static Real staff_space (Grob *);
  static Real get_line_thickness (Grob *);
  static Real get_ledger_line_thickness (Grob *);

  static std::vector<Real> line_positions (Grob *);
  static std::vector<Real> ledger_positions (Grob *me, int pos,
                                             Item const *head = 0);
  static bool on_line (Grob *me, int pos, bool allow_ledger = true);
  static Interval line_span (Grob *);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));

private:
  static int internal_line_count (Grob *);
};
#endif // STAFF_SYMBOL_HH
