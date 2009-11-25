/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef ACCIDENTAL_PLACEMENT_HH
#define ACCIDENTAL_PLACEMENT_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Accidental_placement
{
public:
  DECLARE_SCHEME_CALLBACK (alignment_callback, (SCM element));
  static void add_accidental (Grob *, Grob *);

  static vector<Grob*> get_relevant_accidentals (vector<Grob*> const &elts, Grob *left);
  static void split_accidentals (Grob *accs,
				 vector<Grob*> *break_reminder,
				 vector<Grob*> *real_acc);

  DECLARE_SCHEME_CALLBACK(calc_positioning_done, (SCM));
  DECLARE_GROB_INTERFACE();
};
#endif /* ACCIDENTAL_PLACEMENT_HH */

