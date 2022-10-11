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

#include "stencil.hh"

#include "main.hh"
#include "font-metric.hh"
#include "input.hh"
#include "string-convert.hh"
#include "warn.hh"

using std::vector;

Stencil::Stencil ()
{
  expr_ = SCM_EOL;
  set_empty (true);
}

Stencil::Stencil (Box b, SCM func)
{
  expr_ = func;
  dim_ = b;
}

SCM
Stencil::mark_smob () const
{
  return expr_;
}

const char *const Stencil::type_p_name_ = "ly:stencil?";

Interval
Stencil::extent (Axis a) const
{
  return dim_[a];
}

bool
Stencil::is_empty () const
{
  return (scm_is_null (expr_) || dim_.is_empty ());
}

bool
Stencil::is_empty (Axis a) const
{
  return dim_.is_empty (a);
}

SCM
Stencil::expr () const
{
  return expr_;
}

Box
Stencil::extent_box () const
{
  return dim_;
}

void
Stencil::rotate (Real a, Offset off)
{
  rotate_degrees (a, off);
}

/*
  Rotate this stencil around the point ABSOLUTE_OFF.

 */
void
Stencil::rotate_degrees_absolute (Real a, Offset absolute_off)
{
  const Real x = absolute_off[X_AXIS];
  const Real y = absolute_off[Y_AXIS];

  /*
   * Build scheme expression (processed in stencil-interpret.cc)
   */
  /* TODO: by hanwenn 2008/09/10 14:38:56:
   * in effect, this copies the underlying expression.  It might be a
   * little bit nicer to mirror this in the api, ie. make a
   *         Stencil::rotated()
   * and have Stencil::rotate be an abbrev of
   *         *this = rotated()
   */

  expr_
    = ly_list (ly_symbol2scm ("rotate-stencil"),
               ly_list (to_scm (a), scm_cons (to_scm (x), to_scm (y))), expr_);

  /*
   * Calculate the new bounding box
   */
  Box shifted_box = extent_box ();
  shifted_box.translate (-absolute_off);

  vector<Offset> pts;
  pts.push_back (
    Offset (shifted_box.x ().at (LEFT), shifted_box.y ().at (DOWN)));
  pts.push_back (
    Offset (shifted_box.x ().at (RIGHT), shifted_box.y ().at (DOWN)));
  pts.push_back (
    Offset (shifted_box.x ().at (RIGHT), shifted_box.y ().at (UP)));
  pts.push_back (Offset (shifted_box.x ().at (LEFT), shifted_box.y ().at (UP)));

  const Offset rot (offset_directed (a));
  dim_.set_empty ();
  for (vsize i = 0; i < pts.size (); i++)
    dim_.add_point (pts[i] * rot + absolute_off);
}

/*
  Rotate this stencil around the point RELATIVE_OFF.

  RELATIVE_OFF is measured in terms of the extent of the stencil, so
  -1 = LEFT/DOWN edge, 1 = RIGHT/UP edge.
 */
void
Stencil::rotate_degrees (Real a, Offset relative_off)
{
  /*
   * Calculate the center of rotation
   */
  const Real x = extent (X_AXIS).linear_combination (relative_off[X_AXIS]);
  const Real y = extent (Y_AXIS).linear_combination (relative_off[Y_AXIS]);
  rotate_degrees_absolute (a, Offset (x, y));
}

void
Stencil::translate (Offset o)
{
  for (const auto a : {X_AXIS, Y_AXIS})
    {
      if (std::isinf (o[a])
          || std::isnan (o[a])

          // ugh, hardcoded.
          || fabs (o[a]) > 1e6)
        {
          programming_error (
            String_convert::form_string (
              "Improbable offset for stencil: %f staff space", o[a])
            + "\n" + "Setting to zero.");
          o[a] = 0.0;
          if (strict_infinity_checking)
            scm_misc_error (__FUNCTION__, "Improbable offset.", SCM_EOL);
        }
    }

  if (!scm_is_null (expr_))
    expr_ = ly_list (ly_symbol2scm ("translate-stencil"), to_scm (o), expr_);
  dim_.translate (o);
}

void
Stencil::translate_axis (Real x, Axis a)
{
  Offset o (0, 0);
  o[a] = x;
  translate (o);
}

void
Stencil::scale (Real x, Real y)
{
  expr_ = ly_list (ly_symbol2scm ("scale-stencil"),
                   ly_list (to_scm (x), to_scm (y)), expr_);
  dim_[X_AXIS] *= x;
  dim_[Y_AXIS] *= y;
}

void
Stencil::add_stencil (Stencil const &s)
{
  SCM cs = ly_symbol2scm ("combine-stencil");
  if (scm_is_null (expr_))
    expr_ = s.expr_;
  else if (scm_is_null (s.expr_))
    ;
  else if (scm_is_pair (expr_) && scm_is_eq (cs, scm_car (expr_)))
    {
      if (scm_is_pair (s.expr_) && scm_is_eq (cs, scm_car (s.expr_)))
        expr_ = ly_append (s.expr_, scm_cdr (expr_));
      else
        expr_ = scm_cons2 (cs, s.expr_, scm_cdr (expr_));
    }
  else
    {
      if (scm_is_pair (s.expr_) && scm_is_eq (cs, scm_car (s.expr_)))
        expr_ = ly_append (s.expr_, ly_list (expr_));
      else
        expr_ = ly_list (cs, s.expr_, expr_);
    }
  dim_.unite (s.dim_);
}

void
Stencil::set_empty (bool e)
{
  if (e)
    {
      dim_[X_AXIS].set_empty ();
      dim_[Y_AXIS].set_empty ();
    }
  else
    {
      dim_[X_AXIS] = Interval (0, 0);
      dim_[Y_AXIS] = Interval (0, 0);
    }
}

void
Stencil::align_to (Axis a, Real x)
{
  if (is_empty (a))
    return;

  Interval i (extent (a));
  translate_axis (-i.linear_combination (x), a);
}

/*  See scheme Function.  */

// Any stencil that is empty in the orthogonal axis is spacing.
// Spacing is not subjected to the std::max (0) rule and can thus be
// negative.

void
Stencil::add_at_edge (Axis a, Direction d, Stencil const &s, Real padding)
{
  // Material that is empty in the axis of reference has only limited
  // usefulness for combining.  We still retain as much information as
  // available since there may be uses like setting page links or
  // background color or watermarks, and off-axis extents.

  if (is_empty (a))
    {
      add_stencil (s);
      return;
    }

  Interval first_extent = extent (a);

  if (s.is_empty (a))
    {
      Stencil toadd (s);
      // translation does not affect axis-empty extent box.
      toadd.translate_axis (first_extent[d], a);
      add_stencil (toadd);
      return;
    }

  Interval next_extent = s.extent (a);

  bool first_is_spacing = is_empty (other_axis (a));
  bool next_is_spacing = s.is_empty (other_axis (a));

  Real offset = first_extent[d] - next_extent[-d];

  if (!(first_is_spacing || next_is_spacing))
    {
      offset += d * padding;
    }

  Stencil toadd (s);
  toadd.translate_axis (offset, a);
  add_stencil (toadd);
}

// Stencil::stack is mainly used for assembling lines or columns
// of stencils.  For the most common case of adding at the right, the
// reference point of the added stencil is usually placed at the right
// edge of the current one, unless the added stencil has a negative
// left extent in which case its left edge is placed at the right edge
// of the current one.
//
// Spacing is special in that it is applied without padding.  Spacing
// at the right edge shifts the right edge accordingly.
//
// For spacing at the left edge, there are several approaches.  In
// order to get to predictable behavior, we want to have at least a
// continuous approach.  An obvious idea is to do a "translate" by the
// appropriate amount.  Doing that while retaining the nominal left
// edge seems like the most straightforward way.

void
Stencil::stack (Axis a, Direction d, Stencil const &s, Real padding,
                Real mindist)
{
  // Material that is empty in the axis of reference can't be sensibly
  // stacked.  We just revert to add_at_edge behavior then.

  if (is_empty (a))
    {
      Stencil toadd (s);
      toadd.add_stencil (*this);
      expr_ = toadd.expr ();
      dim_ = toadd.extent_box ();
      return;
    }

  Interval first_extent = extent (a);

  if (s.is_empty (a))
    {
      Stencil toadd (s);
      toadd.translate_axis (first_extent[d], a);
      toadd.add_stencil (*this);
      expr_ = toadd.expr ();
      dim_ = toadd.extent_box ();
      return;
    }

  Interval next_extent = s.extent (a);

  // It is somewhat tedious to special-case all spacing, but it turns
  // out that not doing so makes it astonishingly hard to make the
  // code do the correct thing.

  // If first is spacing, we translate second accordingly without
  // letting this affect its backward edge.
  if (is_empty (other_axis (a)))
    {
      Stencil toadd (s);
      // Spacing assigns meaning to "intervals" with negative extent,
      // so we cannot use first_extent.length () here
      Real offset = first_extent[d] - first_extent[-d];
      toadd.translate_axis (offset, a);
      toadd.add_stencil (*this);
      expr_ = toadd.expr ();
      dim_ = toadd.extent_box ();
      dim_[a][-d] = next_extent[-d];
      dim_[a][d] = next_extent[d] + offset;
      return;
    }

  // If next is spacing, similar action:
  if (s.is_empty (other_axis (a)))
    {
      Stencil toadd (s);
      Real offset = first_extent[d];
      toadd.translate_axis (offset, a);
      toadd.add_stencil (*this);
      expr_ = toadd.expr ();
      dim_ = toadd.extent_box ();
      dim_[a][-d] = first_extent[-d];
      dim_[a][d] = first_extent[d] + next_extent[d] - next_extent[-d];
      return;
    }

  Real offset = first_extent[d];

  // If the added stencil has a backwardly protruding edge, we make
  // room for it when combining.

  if (d * next_extent[-d] < 0)
    offset -= next_extent[-d];

  offset += d * padding;

  if (offset * d < mindist)
    offset = d * mindist;

  Stencil toadd (s);
  toadd.translate_axis (offset, a);
  toadd.add_stencil (*this);
  expr_ = toadd.expr ();
  dim_ = toadd.extent_box ();
  dim_[a][-d] = first_extent[-d];
  dim_[a][d] = next_extent[d] + offset;
}

Stencil
Stencil::in_color (Real r, Real g, Real b, Real a) const
{

  Stencil new_stencil (
    extent_box (),
    ly_list (ly_symbol2scm ("color"),
             scm_list_n (to_scm (r), to_scm (g), to_scm (b),
                         a == 1.0 ? SCM_UNDEFINED : to_scm (a), SCM_UNDEFINED),
             expr ()));
  return new_stencil;
}

/* convenience */
Stencil
Stencil::translated (Offset z) const
{
  Stencil s (*this);
  s.translate (z);
  return s;
}

Stencil
Stencil::with_outline (Stencil const &ol) const
{
  Stencil new_stencil (
    ol.extent_box (),
    ly_list (ly_symbol2scm ("with-outline"), ol.expr (), expr ()));
  return new_stencil;
}
