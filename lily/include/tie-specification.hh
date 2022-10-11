/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#ifndef TIE_SPECIFICATION_HH
#define TIE_SPECIFICATION_HH

#include "lily-proto.hh"
#include "drul-array.hh"

struct Tie_specification
{
  int position_;
  Drul_array<Grob *> note_head_drul_;
  Drul_array<int> column_ranks_;
  Grob *tie_grob_;

  bool has_manual_position_;
  bool has_manual_dir_;
  bool has_manual_delta_y_;
  bool has_accidental_;

  Real manual_position_;
  Direction manual_dir_;

  Tie_specification ();
  int column_span () const;
  void from_grob (Grob *);
};

#endif /* TIE_SPECIFICATION_HH */
