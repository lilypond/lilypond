/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys

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

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH

#include "stencil.hh"
#include "grob-interface.hh"

class Note_head
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (stem_x_shift, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_attachment, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_tab_stem_attachment, (SCM));
  DECLARE_SCHEME_CALLBACK (include_ledger_line_height, (SCM));

  static Real stem_attachment_coordinate (Grob *, Axis a);
  static int get_balltype (Grob *);

  static Offset get_stem_attachment (Font_metric *, const std::string &,
                                     Direction);
};
#endif // NOTEHEAD_HH
