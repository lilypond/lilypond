/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2014--2022 David Kastrup <dak@gnu.org>

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

// This module is concerned with managing grob-properties (more
// exactly, grob property templates, as they are not yet part of a
// grob) inside of context properties, in a context-hierarchical
// manner, with one stack for properties and subproperties per
// context.

#ifndef GROB_PROPERTIES_HH
#define GROB_PROPERTIES_HH

#include "lily-proto.hh"

// Several algorithms on Grob_properties need self-identifying
// information to work properly, but there is no point in storing them
// in the Grob_properties data structure itself.  Instead we create a
// reflective data structure containing all necessary information for
// the algorithms processing Grob_properties.

class Grob_property_info
{
  Context *const context_;
  SCM const symbol_;
  Grob_properties *props_;

public:
  Grob_property_info (Context *context, SCM symbol, Grob_properties *props = 0)
    : context_ (context),
      symbol_ (symbol),
      props_ (props)
  {
  }
  operator bool () { return props_; }
  Grob_property_info find ();
  bool check ();
  bool create ();
  SCM updated ();
  SCM push (SCM path, SCM value);
  SCM temporary_override (SCM path, SCM value);
  SCM temporary_revert (SCM path);
  void matched_pop (SCM);
  void pop (SCM path);
  void pushpop (SCM path, SCM value)
  {
    if (SCM_UNBNDP (value))
      return pop (path);
    push (path, value);
  }
};
#endif
