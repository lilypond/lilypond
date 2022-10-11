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

#include "font-metric.hh"
#include "international.hh"
#include "lookup.hh"
#include "ly-scm-list.hh"
#include "offset.hh"
#include "string-convert.hh"

#include <unordered_map>

using std::vector;

/*
 * A few general helpers in degrees
 */

LY_DEFINE (ly_angle, "ly:angle", 1, 1, 0, (SCM x, SCM y),
           R"(
Calculate angle in degrees of given vector.  With one argument, @var{x}@tie{}is
a number pair indicating the vector.  With two arguments, @var{x}
and@tie{}@var{y} specify the respective coordinates.
           )")
{
  Offset off;
  if (SCM_UNBNDP (y))
    {
      LY_ASSERT_TYPE (is_number_pair, x, 1);
      off = from_scm<Offset> (x);
    }
  else
    {
      LY_ASSERT_TYPE (scm_is_number, x, 1);
      LY_ASSERT_TYPE (scm_is_number, y, 2);
      off = Offset (from_scm<double> (x), from_scm<double> (y));
    }
  return to_scm (off.angle_degrees ());
}

LY_DEFINE (ly_length, "ly:length", 1, 1, 0, (SCM x, SCM y),
           R"(
Calculate magnitude of given vector.  With one argument, @var{x}@tie{}is a
number pair indicating the vector.  With two arguments,
@var{x}@tie{}and@tie{}@var{y} specify the respective coordinates.
           )")
{
  Offset off;
  if (SCM_UNBNDP (y))
    {
      LY_ASSERT_TYPE (is_number_pair, x, 1);
      off = from_scm<Offset> (x);
    }
  else
    {
      LY_ASSERT_TYPE (scm_is_number, x, 1);
      LY_ASSERT_TYPE (scm_is_number, y, 2);
      off = Offset (from_scm<double> (x), from_scm<double> (y));
    }
  return to_scm (off.length ());
}

LY_DEFINE (ly_directed, "ly:directed", 1, 1, 0, (SCM direction, SCM magnitude),
           R"(
Calculate an @code{(x . y)} pair with optional @var{magnitude} (defaulting to
@code{1.0}) and @var{direction} specified either as an angle in degrees or a
coordinate pair giving the direction.  If @var{magnitude} is a pair, the
respective coordinates are scaled independently, useful for ellipse drawings.
           )")
{
  Offset res;
  if (scm_is_pair (direction))
    {
      LY_ASSERT_TYPE (is_number_pair, direction, 1);
      res = from_scm<Offset> (direction).direction ();
    }
  else
    {
      LY_ASSERT_TYPE (scm_is_number, direction, 1);
      res = offset_directed (from_scm<double> (direction));
    }
  if (SCM_UNBNDP (magnitude))
    return to_scm (res);
  if (scm_is_pair (magnitude))
    {
      LY_ASSERT_TYPE (is_number_pair, magnitude, 2);
      return to_scm (res.scale (from_scm<Offset> (magnitude)));
    }
  LY_ASSERT_TYPE (scm_is_number, magnitude, 2);
  return to_scm (from_scm<double> (magnitude) * res);
}

/*
  TODO: naming add/combine.
*/

LY_DEFINE (ly_stencil_translate_axis, "ly:stencil-translate-axis", 3, 0, 0,
           (SCM stil, SCM amount, SCM axis),
           R"(
Return a copy of stencil @var{stil} but translated by @var{amount} in
@var{axis} direction.
           )")
{
  auto s = *LY_ASSERT_SMOB (const Stencil, stil, 1);
  LY_ASSERT_TYPE (scm_is_number, amount, 2);

  LY_ASSERT_TYPE (is_scm<Axis>, axis, 3);

  Real real_amount = from_scm<double> (amount);

  s.translate_axis (real_amount, from_scm<Axis> (axis));
  return s.smobbed_copy ();
}

LY_DEFINE (ly_stencil_translate, "ly:stencil-translate", 2, 0, 0,
           (SCM stil, SCM offset),
           R"(
Return a copy of stencil @var{stil} but translated by @var{offset} (a pair of
numbers).
           )")
{
  auto s = *LY_ASSERT_SMOB (const Stencil, stil, 1);
  LY_ASSERT_TYPE (is_number_pair, offset, 2);
  Offset o = from_scm<Offset> (offset);

  s.translate (o);
  return s.smobbed_copy ();
}

LY_DEFINE (ly_stencil_expr, "ly:stencil-expr", 1, 0, 0, (SCM stil),
           R"(
Return the expression of stencil @var{stil}.
           )")
{
  auto *const s = LY_ASSERT_SMOB (const Stencil, stil, 1);
  return s->expr ();
}

LY_DEFINE (ly_stencil_extent, "ly:stencil-extent", 2, 0, 0,
           (SCM stil, SCM axis),
           R"(
Return a pair of numbers signifying the extent of stencil @var{stil} in
@var{axis} direction (@code{0} or@tie{}@code{1} for x and y@tie{}axis,
respectively).
           )")
{
  auto *const s = LY_ASSERT_SMOB (const Stencil, stil, 1);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);

  return to_scm (s->extent (from_scm<Axis> (axis)));
}

LY_DEFINE (ly_stencil_empty_p, "ly:stencil-empty?", 1, 1, 0,
           (SCM stil, SCM axis),
           R"(
Return whether @var{stil} is empty.  If an optional @var{axis} is supplied, the
emptiness check is restricted to that axis.
           )")
{
  auto *const s = LY_ASSERT_SMOB (const Stencil, stil, 1);
  if (SCM_UNBNDP (axis))
    return to_scm (s->is_empty ());
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  return to_scm (s->is_empty (from_scm<Axis> (axis)));
}

LY_DEFINE (ly_stencil_combine_at_edge, "ly:stencil-combine-at-edge", 4, 1, 0,
           (SCM first, SCM axis, SCM direction, SCM second, SCM padding),
           R"(
Construct a stencil by putting @var{second} next to @var{first}.  @var{axis}
can be @code{0} (x@tie{}axis) or@tie{}@code{1} (y@tie{}axis).  @var{direction}
can be @code{-1} (left or down) or@tie{}@code{1} (right or up).  The stencils
are juxtaposed with @var{padding} as extra space.  @var{first} and @var{second}
may also be @code{'()} or @code{#f}.
           )")
{
  auto *const s1 = unsmob<const Stencil> (first);
  auto *const s2 = unsmob<const Stencil> (second);
  Stencil result;

  SCM_ASSERT_TYPE (s1 || scm_is_false (first) || scm_is_null (first), first,
                   SCM_ARG1, __FUNCTION__, "Stencil, #f or ()");
  SCM_ASSERT_TYPE (s2 || scm_is_false (second) || scm_is_null (second), second,
                   SCM_ARG4, __FUNCTION__, "Stencil, #f or ()");
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  LY_ASSERT_TYPE (is_scm<Direction>, direction, 3);

  Real p = 0.0;
  if (!SCM_UNBNDP (padding))
    {
      LY_ASSERT_TYPE (scm_is_number, padding, 5);
      p = from_scm<double> (padding);
    }

  if (s1)
    result = *s1;

  if (s2)
    result.add_at_edge (from_scm<Axis> (axis),
                        Direction (from_scm<int> (direction)), *s2, p);

  scm_remember_upto_here_2 (first, second);

  return result.smobbed_copy ();
}

LY_DEFINE (ly_stencil_stack, "ly:stencil-stack", 4, 2, 0,
           (SCM first, SCM axis, SCM direction, SCM second, SCM padding,
            SCM mindist),
           R"(
Construct a stencil by stacking @var{second} next to @var{first}.  @var{axis}
can be @code{0} (x@tie{}axis) or@tie{}@code{1} (y@tie{}axis).  @var{direction}
can be @code{-1} (left or down) or@tie{}@code{1} (right or up).  The stencils
are juxtaposed with @var{padding} as extra space.  @var{first} and @var{second}
may also be @code{'()} or @code{#f}.  As opposed to
@code{ly:@/stencil-combine-at-edge}, metrics are suited for successively
accumulating lines of stencils.  Also, @var{second} stencil is drawn last.

If @var{mindist} is specified, reference points are placed apart at least by
this distance.  If either of the stencils is spacing, @var{padding} and
@var{mindist} do not apply.
           )")
{
  auto *const s1 = unsmob<const Stencil> (first);
  auto *const s2 = unsmob<const Stencil> (second);
  Stencil result;

  SCM_ASSERT_TYPE (s1 || scm_is_false (first) || scm_is_null (first), first,
                   SCM_ARG1, __FUNCTION__, "Stencil, #f or ()");
  SCM_ASSERT_TYPE (s2 || scm_is_false (second) || scm_is_null (second), second,
                   SCM_ARG4, __FUNCTION__, "Stencil, #f or ()");
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  LY_ASSERT_TYPE (is_scm<Direction>, direction, 3);

  Real p = 0.0;
  if (!SCM_UNBNDP (padding))
    {
      LY_ASSERT_TYPE (scm_is_number, padding, 5);
      p = from_scm<double> (padding);
    }
  Real d = -infinity_f;
  if (!SCM_UNBNDP (mindist))
    {
      LY_ASSERT_TYPE (scm_is_number, mindist, 6);
      d = from_scm<double> (mindist);
    }

  if (s1)
    result = *s1;

  if (s2)
    result.stack (from_scm<Axis> (axis), Direction (from_scm<int> (direction)),
                  *s2, p, d);

  scm_remember_upto_here_2 (first, second);

  return result.smobbed_copy ();
}

LY_DEFINE (ly_stencil_add, "ly:stencil-add", 0, 0, 1, (SCM args),
           R"(
Combine stencils.  Takes any number of arguments.
           )")
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_REST_ARGUMENT (args);

  SCM expr = SCM_EOL;
  SCM cs = ly_symbol2scm ("combine-stencil");

  Box extent;
  extent.set_empty ();

  for (SCM scm_s : as_ly_scm_list (args))
    {
      auto *const s = unsmob<const Stencil> (scm_s);
      if (!s)
        SCM_ASSERT_TYPE (s, scm_s, SCM_ARGn, __FUNCTION__, "Stencil");

      extent.unite (s->extent_box ());
      if (scm_is_pair (s->expr ()) && scm_is_eq (cs, s->expr ()))
        {
          expr = scm_reverse_x (scm_list_copy (scm_cdr (s->expr ())), expr);
        }
      else
        expr = scm_cons (s->expr (), expr);
    }

  expr = scm_cons (cs, scm_reverse_x (expr, SCM_EOL));
  return Stencil (extent, expr).smobbed_copy ();
}

LY_DEFINE (ly_make_stencil, "ly:make-stencil", 1, 2, 0,
           (SCM expr, SCM xext, SCM yext),
           R"(
Stencils are device independent output expressions.  They carry two pieces of
information:

@enumerate
@item
A specification of how to print this object.  This specification is processed
by the output backends, for example @file{scm/output-ps.scm}.

@item
The vertical and horizontal extents of the object, given as pairs.  If an
extent is unspecified (or if you use @code{empty-interval} as its value), it is
taken to be empty.
@end enumerate
           )")
{
  SCM_ASSERT_TYPE (!scm_is_pair (expr) || is_stencil_head (scm_car (expr)),
                   expr, SCM_ARG1, __FUNCTION__,
                   "registered stencil expression");

  Interval x;
  if (!SCM_UNBNDP (xext))
    {
      LY_ASSERT_TYPE (is_number_pair, xext, 2);
      x = from_scm<Interval> (xext);
    }

  Interval y;
  if (!SCM_UNBNDP (yext))
    {
      LY_ASSERT_TYPE (is_number_pair, yext, 3);
      y = from_scm<Interval> (yext);
    }

  Box b (x, y);
  Stencil s (b, expr);
  return s.smobbed_copy ();
}

LY_DEFINE (ly_stencil_aligned_to, "ly:stencil-aligned-to", 3, 0, 0,
           (SCM stil, SCM axis, SCM dir),
           R"(
Align stencil @var{stil} using its own extents.  @var{dir} is a number.
@w{@code{-1}} and @code{1} are left and right, respectively.  Other values are
interpolated (so @code{0} means the center).
           )")
{
  auto target = *LY_ASSERT_SMOB (const Stencil, stil, 1);
  LY_ASSERT_TYPE (is_scm<Axis>, axis, 2);
  LY_ASSERT_TYPE (scm_is_number, dir, 3);

  target.align_to (from_scm<Axis> (axis), from_scm<double> (dir));
  return target.smobbed_copy ();
}

struct Stencil_color
{
  Real rgba_[4] = {0, 0, 0, 1};
};

static std::unordered_map<std::string, Stencil_color> named_colors;

LY_DEFINE (ly_set_color_names, "ly:set-color-names", 1, 0, 0, (SCM alist),
           R"(
Define named colors for @code{ly:stencil-in-color}. @var{alist} has the entries
of the format @code{(@var{name} . @var{color})}, where @var{color} is a list of
length 3 (RGB) or 4 (RGB+alpha).
           )")
{
  named_colors.clear ();
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      SCM name = scm_car (entry);

      LY_ASSERT_TYPE (scm_is_string, name, 1);
      int sz = 0;

      Stencil_color color;
      for (SCM c = scm_cdr (entry); scm_is_pair (c) && sz < 4;
           c = scm_cdr (c), sz++)
        {
          Real v = from_scm<Real> (scm_car (c), -1);
          if (v < 0 || v > 1)
            scm_wrong_type_arg_msg (
              mangle_cxx_identifier (__FUNCTION__).c_str (), 1, scm_car (c),
              "list of 3 or 4 numbers in [0.0,1.0]");

          color.rgba_[sz] = v;
        }

      if (sz != 3 && sz != 4)
        {
          scm_wrong_type_arg_msg (mangle_cxx_identifier (__FUNCTION__).c_str (),
                                  1, scm_cdr (entry), "list of 3 or 4 numbers");
        }

      named_colors[String_convert::to_lower (ly_scm2string (name))] = color;
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_stencil_in_color, "ly:stencil-in-color", 2, 3, 0,
           (SCM stc, SCM r, SCM g, SCM b, SCM a),
           R"(
Put stencil @var{stc} in a different color.  Accepts either three values for
@var{r}, @var{g}, @var{b} and an optional value for @var{a}, or a single
CSS-like string.
           )")
{
  auto *const stil = LY_ASSERT_SMOB (const Stencil, stc, 1);

  if (SCM_UNBNDP (g) && scm_is_string (r))
    {
      return stil->in_color (ly_scm2string (r)).smobbed_copy ();
    }

  LY_ASSERT_TYPE (is_scm<Real>, r, 1);
  LY_ASSERT_TYPE (is_scm<Real>, g, 2);
  LY_ASSERT_TYPE (is_scm<Real>, b, 3);
  if (!SCM_UNBNDP (a))
    LY_ASSERT_TYPE (is_scm<Real>, a, 4);

  return stil
    ->in_color (from_scm<Real> (r), from_scm<Real> (g), from_scm<Real> (b),
                SCM_UNBNDP (a) ? 1.0 : from_scm<Real> (a))
    .smobbed_copy ();
}

Stencil
Stencil::in_color (std::string const &name) const
{
  if (name.empty ())
    return *this;

  Stencil_color c;
  if (name[0] == '#')
    {
      char hex_rgba[9] = "000000FF";

      if (name.length () < 7)
        {
          // #rgb(a) => #rrggbb(aa)
          for (size_t i = 0; i + 1 < name.size () && i < 4; i++)
            {
              hex_rgba[2 * i] = hex_rgba[2 * i + 1] = name[i + 1];
            }
        }
      else
        {
          for (size_t i = 0; i + 1 < name.size () && i < 8; i++)
            hex_rgba[i] = name[i + 1];
        }

      for (int i = 0; i < 4; i++)
        {
          int c1 = String_convert::hex2nibble (hex_rgba[2 * i]);
          int c2 = String_convert::hex2nibble (hex_rgba[2 * i + 1]);
          if (c1 == -1 || c2 == -1)
            {
              warning (_f ("invalid sequence %c%c in color "
                           "(characters should be in one of ranges "
                           "0-9, a-f, A-F)",
                           hex_rgba[2 * i], hex_rgba[2 * i + 1]));
              c1 = c2 = 0;
            }
          c.rgba_[i] = Real (c1 * 16 + c2) / 255.0;
        }
    }
  else
    c = named_colors[String_convert::to_lower (name)];

  return in_color (c.rgba_[0], c.rgba_[1], c.rgba_[2], c.rgba_[3]);
}

LY_DEFINE (ly_bracket, "ly:bracket", 4, 0, 0, (SCM a, SCM iv, SCM t, SCM p),
           R"(
Make a bracket in direction@tie{}@var{a}.  The extent of the bracket is given
by @var{iv}.  The wings protrude by an amount of@tie{}@var{p}, which may be
negative.  The thickness is given by@tie{}@var{t}.
           )")
{
  LY_ASSERT_TYPE (is_scm<Axis>, a, 1);
  LY_ASSERT_TYPE (is_number_pair, iv, 2);
  LY_ASSERT_TYPE (scm_is_number, t, 3);
  LY_ASSERT_TYPE (scm_is_number, p, 4);

  Interval extent = from_scm<Interval> (iv);
  if (std::isinf (extent[LEFT]) || std::isinf (extent[RIGHT]))
    {
      programming_error ("bracket extent may not be infinite");
      return Stencil ().smobbed_copy ();
    }

  return Lookup::bracket (from_scm<Axis> (a), extent, from_scm<double> (t),
                          from_scm<double> (p), 0.95 * from_scm<double> (t))
    .smobbed_copy ();
}

LY_DEFINE (ly_stencil_rotate, "ly:stencil-rotate", 4, 0, 0,
           (SCM stil, SCM angle, SCM x, SCM y),
           R"(
Return a stencil @var{stil} rotated by @var{angle} degrees around the relative
offset @w{(@var{x}, @var{y})}.  E.g., an offset of @w{(-1, 1)} rotates the
stencil around the left upper corner.
           )")
{
  auto s = *LY_ASSERT_SMOB (const Stencil, stil, 1);
  LY_ASSERT_TYPE (scm_is_number, angle, 2);
  LY_ASSERT_TYPE (scm_is_number, x, 3);
  LY_ASSERT_TYPE (scm_is_number, y, 4);
  Real a = from_scm<double> (angle);
  Real x_off = from_scm<double> (x);
  Real y_off = from_scm<double> (y);

  s.rotate_degrees (a, Offset (x_off, y_off));
  return s.smobbed_copy ();
}

LY_DEFINE (ly_stencil_rotate_absolute, "ly:stencil-rotate-absolute", 4, 0, 0,
           (SCM stil, SCM angle, SCM x, SCM y),
           R"(
Return a stencil @var{stil} rotated by @var{angle} degrees around point
@w{(@var{x}, @var{y})}, given in absolute coordinates.
           )")
{
  auto s = *LY_ASSERT_SMOB (const Stencil, stil, 1);
  LY_ASSERT_TYPE (scm_is_number, angle, 2);
  LY_ASSERT_TYPE (scm_is_number, x, 3);
  LY_ASSERT_TYPE (scm_is_number, y, 4);
  Real a = from_scm<double> (angle);
  Real x_off = from_scm<double> (x);
  Real y_off = from_scm<double> (y);

  s.rotate_degrees_absolute (a, Offset (x_off, y_off));
  return s.smobbed_copy ();
}

LY_DEFINE (ly_round_filled_box, "ly:round-filled-box", 3, 0, 0,
           (SCM xext, SCM yext, SCM blot),
           R"(
Make a @code{Stencil} object that prints a black box of dimensions @var{xext},
@var{yext} and roundness @var{blot}.
           )")
{
  LY_ASSERT_TYPE (is_number_pair, xext, 1);
  LY_ASSERT_TYPE (is_number_pair, yext, 2);
  LY_ASSERT_TYPE (scm_is_number, blot, 3);

  return Lookup::round_filled_box (
           Box (from_scm<Interval> (xext), from_scm<Interval> (yext)),
           from_scm<double> (blot))
    .smobbed_copy ();
}

LY_DEFINE (ly_round_polygon, "ly:round-polygon", 2, 2, 0,
           (SCM points, SCM blot, SCM extroversion, SCM filled_scm),
           R"(
Make a @code{Stencil} object that prints a black polygon with corners at the
points defined by @var{points} (list of coordinate pairs) and roundness
@var{blot}.  Optional @var{extroversion} shifts the outline outward, with the
default of@tie{}0 keeping the middle of the line just on the polygon.
           )")
{
  SCM_ASSERT_TYPE (scm_ilength (points) > 0, points, SCM_ARG1, __FUNCTION__,
                   "list of coordinate pairs");
  LY_ASSERT_TYPE (scm_is_number, blot, 2);
  Real ext = 0;
  if (!SCM_UNBNDP (extroversion))
    {
      LY_ASSERT_TYPE (scm_is_number, extroversion, 3);
      ext = from_scm<double> (extroversion);
    }
  bool filled = true;
  if (!SCM_UNBNDP (filled_scm))
    {
      LY_ASSERT_TYPE (scm_is_bool, filled_scm, 4);
      filled = from_scm<bool> (filled_scm);
    }
  vector<Offset> pts;
  for (SCM scm_pt : as_ly_scm_list (points))
    {
      if (scm_is_pair (scm_pt))
        {
          pts.push_back (from_scm<Offset> (scm_pt));
        }
      else
        {
          // TODO: Print out warning
        }
    }
  return Lookup::round_polygon (pts, from_scm<double> (blot), ext, filled)
    .smobbed_copy ();
}

LY_DEFINE (ly_register_stencil_expression, "ly:register-stencil-expression", 1,
           0, 0, (SCM symbol),
           R"(
Add @var{symbol} as head of a stencil expression.
           )")
{
  LY_ASSERT_TYPE (ly_is_symbol, symbol, 1);
  register_stencil_head (symbol);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_all_stencil_expressions, "ly:all-stencil-expressions", 0, 0, 0,
           (),
           R"(
Return all symbols recognized as stencil expressions.
           )")
{
  return all_stencil_heads ();
}

LY_DEFINE (ly_stencil_scale, "ly:stencil-scale", 2, 1, 0,
           (SCM stil, SCM x, SCM y),
           R"(
Scale stencil @var{stil} using the horizontal and vertical scaling
factors @var{x} and optional@tie{}@var{y} (defaulting to@tie{}@var{x}).
Negative values flip or mirror @var{stil} without changing its origin;
this may result in collisions unless it is repositioned.
           )")
{
  auto s = *LY_ASSERT_SMOB (const Stencil, stil, 1);
  LY_ASSERT_TYPE (scm_is_number, x, 2);
  if (SCM_UNBNDP (y))
    y = x;
  else
    LY_ASSERT_TYPE (scm_is_number, y, 3);

  s.scale (from_scm<double> (x), from_scm<double> (y));
  return s.smobbed_copy ();
}

LY_DEFINE (ly_stencil_outline, "ly:stencil-outline", 2, 0, 0,
           (SCM stil, SCM outline),
           R"(
Return a stencil with the stencil expression (inking) of stencil @var{stil} but
with outline and dimensions from stencil @var{outline}.
           )")
{
  auto s = *LY_ASSERT_SMOB (const Stencil, stil, 1);
  auto *const o = LY_ASSERT_SMOB (const Stencil, outline, 2);
  return s.with_outline (*o).smobbed_copy ();
}
