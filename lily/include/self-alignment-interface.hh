/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SELF_ALIGNMENT_INTERFACE_HH
#define SELF_ALIGNMENT_INTERFACE_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

struct Self_alignment_interface
{
  DECLARE_GROB_INTERFACE();

  static SCM aligned_on_self (Grob *me, Axis a, bool pure, int start, int end);
  static SCM centered_on_object (Grob *me, Axis a);
  static SCM aligned_on_parent (Grob *me, Axis a);
  static void set_center_parent (Grob *me, Axis a);
  static void set_align_self (Grob *me, Axis a);
  
  DECLARE_SCHEME_CALLBACK (x_aligned_on_self, (SCM element));
  DECLARE_SCHEME_CALLBACK (y_aligned_on_self, (SCM element));

  DECLARE_SCHEME_CALLBACK (pure_y_aligned_on_self, (SCM element, SCM start, SCM end));

  DECLARE_SCHEME_CALLBACK (centered_on_x_parent, (SCM element));
  DECLARE_SCHEME_CALLBACK (centered_on_y_parent, (SCM element));
  DECLARE_SCHEME_CALLBACK (x_centered_on_y_parent, (SCM element));

  DECLARE_SCHEME_CALLBACK (aligned_on_x_parent, (SCM element));
  DECLARE_SCHEME_CALLBACK (aligned_on_y_parent, (SCM element));
};
#endif
