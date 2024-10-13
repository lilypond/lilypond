/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2024 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#ifndef DEPRECATED_PROPERTY_HH
#define DEPRECATED_PROPERTY_HH

#include "lily-guile.hh"

// This helps deal with deprecated properties.  Access to deprecated properties
// should go through this class to ensure that lilypond logs a warning on the
// first use of particular deprecated properties.
class Deprecated_property
{
public:
  // Return a list of parameters describing how to get a new property in lieu of
  // a deprecated property, or #f if no information is availble.
  //
  // sym is a symbol that potentially identifies a deprecated property.
  //
  // obj_prop is an object property (a procedure-with-setter) to apply to the
  // symbol.  The different property categories (backend, music, translation)
  // should use distinct object properties for this purpose so that a symbol can
  // be used differently in those different contexts.
  static SCM getter_desc (SCM sym, SCM obj_prop);

  // Return a list of parameters describing how to set a new property in lieu of
  // a deprecated property, or #f if no information is availble.
  //
  // sym is a symbol that potentially identifies a deprecated property.
  //
  // obj_prop is an object property (a procedure-with-setter) to apply to the
  // symbol.  The different property categories (backend, music, translation)
  // should use distinct object properties for this purpose so that a symbol can
  // be used differently in those different contexts.
  static SCM setter_desc (SCM sym, SCM obj_prop);
};

#endif /* DEPRECATED_PROPERTY_HH */
