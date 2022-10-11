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
#ifndef STENCIL_HH
#define STENCIL_HH

#include <cstdlib> // size_t

#include "lily-proto.hh"
#include "box.hh"
#include "smobs.hh"

/** a group of individually translated symbols. You can add stencils
    to the top, to the right, etc.

    It is implemented as a "tree" of scheme expressions, as in

    Expr = combine Expr-list
    | translate Offset Expr
    | origin (ORIGIN) Expr
    | no-origin Expr
    | (SCHEME)
    ;

    SCHEME is a Scheme expression that --when eval'd-- produces the
    desired output.

    Notes:

    * Because of the way that Stencil is implemented, it is the most
    efficient to add "fresh" stencils to what you're going to build.

    * Do not create Stencil objects on the heap or attempt to share
    or transfer ownership by pointer.  Either copy Stencil objects
    or use SCM references.

    * Empty stencils have empty dimensions.  If add_at_edge is used to
    init the stencil, we assume that

    DIMENSIONS = (Interval (0, 0), Interval (0, 0)
*/
class Stencil : public Simple_smob<Stencil>
{
public:
  SCM mark_smob () const;
  static const char *const type_p_name_;

private:
  Box dim_;
  SCM expr_;

public:
  Stencil (Box, SCM s);
  Stencil ();

  SCM expr () const;

  /**
     Set dimensions to empty, or to (Interval (0, 0), Interval (0, 0) */
  void set_empty (bool);
  void add_at_edge (Axis a, Direction d, const Stencil &m, Real padding);
  void stack (Axis a, Direction d, const Stencil &m, Real padding,
              Real mindist);
  void add_stencil (Stencil const &m);
  void translate (Offset);
  Stencil translated (Offset) const;
  void rotate (Real, Offset);
  void rotate_degrees (Real, Offset);
  void rotate_degrees_absolute (Real, Offset);
  void align_to (Axis a, Real x);
  void translate_axis (Real, Axis);
  void scale (Real, Real);

  Interval extent (Axis) const;
  Box extent_box () const;
  bool is_empty () const;
  bool is_empty (Axis) const;
  Stencil in_color (Real r, Real g, Real b, Real a = 1.0) const;
  Stencil in_color (std::string const &css_color) const;
  Stencil with_outline (Stencil const &ol) const;
};

void register_stencil_head (SCM symbol);
bool is_stencil_head (SCM symbol);
SCM all_stencil_heads ();

// C++ code must not modify Stencil objects shared with Scheme.
// Requiring unsmob<const Stencil> helps to enforce this.
template <>
inline Stencil *unsmob<Stencil> (SCM) = delete;

#endif /* STENCIL_HH */
