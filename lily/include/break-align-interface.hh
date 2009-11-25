/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef BREAK_ALIGN_INTERFACE_HH
#define BREAK_ALIGN_INTERFACE_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Break_alignment_interface
{
public:
  static vector<Grob*> ordered_elements (Grob *me);
  DECLARE_GROB_INTERFACE();
  static void add_element (Grob *me, Grob *add);
  static SCM break_align_order (Item *me);
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM element));
  DECLARE_SCHEME_CALLBACK (self_align_callback, (SCM element));
};

struct Break_aligned_interface
{
  DECLARE_SCHEME_CALLBACK (calc_average_anchor, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_extent_aligned_anchor, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_break_visibility, (SCM));
  DECLARE_GROB_INTERFACE();
};

struct Break_alignable_interface
{
  DECLARE_SCHEME_CALLBACK (self_align_callback, (SCM element));
  DECLARE_GROB_INTERFACE();
};

#endif // BREAK_ALIGN_INTERFACE_HH
