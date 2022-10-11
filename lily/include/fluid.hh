/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2015--2022 David Kastrup <dak@gnu.org>

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

#ifndef FLUID_HH
#define FLUID_HH

#include "lily-guile.hh"

// Fluid is a wrapper class for cached storage of GUILE fluids.
// You use it like
//
//   Fluid parser (Lily::f_parser);
//
// and when you first access `parser' as an SCM value, its value is
// fetched from the respective fluid (in this case `%parser', cf
// lily/lily-imports.cc) and cached for future accesses.
//
// Since fluids act as implicit function parameters, it can only be
// meaningfully employed for variables of automatic
// (function/block-local) duration.
//
// Once you create a fluid cache, it should be passed around by
// reference in order to keep the performance impact low.

class Fluid
{
  SCM const fluid_; // the fluid itself
  SCM value_;       // its cached value, SCM_UNDEFINED if unset

  Fluid ();              // No accessible default constructor
  Fluid (const Fluid &); // Don't copy
  // Caching fluids only makes sense if we really treat them as
  // function parameters, namely only modify them synchronized to
  // function calls, like when using scm_with_fluid.  So no assignment
  // operator or any other interface to scm_fluid_set_x.
  Fluid &operator= (const Fluid &);

public:
  Fluid (SCM fluid)
    : fluid_ (fluid),
      value_ (SCM_UNDEFINED)
  {
  }
  operator SCM ()
  {
    if (SCM_UNBNDP (value_))
      value_ = scm_fluid_ref (fluid_);
    return value_;
  }
};

#endif
