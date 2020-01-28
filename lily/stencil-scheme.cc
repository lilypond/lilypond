/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "font-metric.hh"
#include "libc-extension.hh"
#include "lookup.hh"
#include "offset.hh"
#include "stencil.hh"

using std::vector;

/*
 * A few general helpers in degrees
 */

LY_DEFINE (
    ly_angle, "ly:angle", 1, 1, 0, (SCM x, SCM y),
    "Calculates angle in degrees of given vector.  With one argument,"
    " @var{x} is a number pair indicating the vector.  With two"
    " arguments, @var{x} and @var{y} specify the respective coordinates.")
{
  Offset off;
  if (SCM_UNBNDP (y))
    {
      LY_ASSERT_TYPE (is_number_pair, x, 1);
      off = ly_scm2offset (x);
    }
  else
    {
      LY_ASSERT_TYPE (scm_is_number, x, 1);
      LY_ASSERT_TYPE (scm_is_number, y, 2);
      off = Offset (scm_to_double (x), scm_to_double (y));
    }
  return scm_from_double (off.angle_degrees ());
}

LY_DEFINE (
    ly_length, "ly:length", 1, 1, 0, (SCM x, SCM y),
    "Calculates magnitude of given vector.  With one argument,"
    " @var{x} is a number pair indicating the vector.  With two"
    " arguments, @var{x} and @var{y} specify the respective coordinates.")
{
  Offset off;
  if (SCM_UNBNDP (y))
    {
      LY_ASSERT_TYPE (is_number_pair, x, 1);
      off = ly_scm2offset (x);
    }
  else
    {
      LY_ASSERT_TYPE (scm_is_number, x, 1);
      LY_ASSERT_TYPE (scm_is_number, y, 2);
      off = Offset (scm_to_double (x), scm_to_double (y));
    }
  return scm_from_double (off.length ());
}

LY_DEFINE (ly_directed, "ly:directed", 1, 1, 0, (SCM direction, SCM magnitude),
           "Calculates an @code{(x . y)} pair with optional @var{magnitude}"
           " (defaulting to @code{1.0}) and @var{direction} specified either"
           " as an angle in degrees or a coordinate pair giving the direction. "
           " If @var{magnitude} is a pair, the respective coordinates are"
           " scaled independently, useful for ellipse drawings.")
{
  Offset res;
  if (scm_is_pair (direction))
    {
      LY_ASSERT_TYPE (is_number_pair, direction, 1);
      res = ly_scm2offset (direction).direction ();
    }
  else
    {
      LY_ASSERT_TYPE (scm_is_number, direction, 1);
      res = offset_directed (scm_to_double (direction));
    }
  if (SCM_UNBNDP (magnitude))
    return ly_offset2scm (res);
  if (scm_is_pair (magnitude))
    {
      LY_ASSERT_TYPE (is_number_pair, magnitude, 2);
      return ly_offset2scm (res.scale (ly_scm2offset (magnitude)));
    }
  LY_ASSERT_TYPE (scm_is_number, magnitude, 2);
  return ly_offset2scm (scm_to_double (magnitude) * res);
}

/*
  TODO: naming add/combine.
*/

LY_DEFINE (ly_stencil_translate_axis, "ly:stencil-translate-axis", 3, 0, 0,
           (SCM stil, SCM amount, SCM axis),
           "Return a copy of @var{stil} but translated by @var{amount}"
           " in @var{axis} direction.")
{
  Stencil *s = unsmob<Stencil> (stil);
  LY_ASSERT_SMOB (Stencil, stil, 1);
  LY_ASSERT_TYPE (scm_is_number, amount, 2);

  LY_ASSERT_TYPE (is_axis, axis, 3);

  Real real_amount = scm_to_double (amount);

  SCM new_s = s->smobbed_copy ();
  scm_remember_upto_here_1 (stil);

  Stencil *q = unsmob<Stencil> (new_s);
  q->translate_axis (real_amount, Axis (scm_to_int (axis)));
  return new_s;
}

LY_DEFINE (ly_stencil_translate, "ly:stencil-translate", 2, 0, 0,
           (SCM stil, SCM offset),
           "Return a @var{stil}, but translated by @var{offset}"
           " (a pair of numbers).")
{
  Stencil *s = unsmob<Stencil> (stil);
  LY_ASSERT_SMOB (Stencil, stil, 1);
  LY_ASSERT_TYPE (is_number_pair, offset, 2);
  Offset o = ly_scm2offset (offset);

  SCM new_s = s->smobbed_copy ();
  scm_remember_upto_here_1 (stil);

  Stencil *q = unsmob<Stencil> (new_s);
  q->translate (o);
  return new_s;
}

LY_DEFINE (ly_stencil_expr, "ly:stencil-expr", 1, 0, 0, (SCM stil),
           "Return the expression of @var{stil}.")
{
  Stencil *s = unsmob<Stencil> (stil);
  LY_ASSERT_SMOB (Stencil, stil, 1);
  return s->expr ();
}

LY_DEFINE (ly_stencil_extent, "ly:stencil-extent", 2, 0, 0,
           (SCM stil, SCM axis),
           "Return a pair of numbers signifying the extent of @var{stil} in"
           " @var{axis} direction (@code{0} or @code{1} for x and"
           " y@tie{}axis, respectively).")
{
  Stencil *s = unsmob<Stencil> (stil);
  LY_ASSERT_SMOB (Stencil, stil, 1);
  LY_ASSERT_TYPE (is_axis, axis, 2);

  return ly_interval2scm (s->extent (Axis (scm_to_int (axis))));
}

LY_DEFINE (ly_stencil_empty_p, "ly:stencil-empty?", 1, 1, 0,
           (SCM stil, SCM axis),
           "Return whether @var{stil} is empty.  If an optional"
           " @var{axis} is supplied, the emptiness check is"
           " restricted to that axis.")
{
  Stencil *s = unsmob<Stencil> (stil);
  LY_ASSERT_SMOB (Stencil, stil, 1);
  if (SCM_UNBNDP (axis))
    return scm_from_bool (s->is_empty ());
  LY_ASSERT_TYPE (is_axis, axis, 2);
  return scm_from_bool (s->is_empty (Axis (scm_to_int (axis))));
}

LY_DEFINE (ly_stencil_combine_at_edge, "ly:stencil-combine-at-edge", 4, 1, 0,
           (SCM first, SCM axis, SCM direction, SCM second, SCM padding),
           "Construct a stencil by putting @var{second} next to @var{first}."
           "  @var{axis} can be 0 (x-axis) or@tie{}1 (y-axis)."
           "  @var{direction} can be -1 (left or down) or@tie{}1 (right or"
           " up).  The stencils are juxtaposed with @var{padding} as extra"
           " space.  @var{first} and @var{second} may also be @code{'()} or"
           " @code{#f}.")
{
  Stencil *s1 = unsmob<Stencil> (first);
  Stencil *s2 = unsmob<Stencil> (second);
  Stencil result;

  SCM_ASSERT_TYPE (s1 || scm_is_false (first) || scm_is_null (first), first,
                   SCM_ARG1, __FUNCTION__, "Stencil, #f or ()");
  SCM_ASSERT_TYPE (s2 || scm_is_false (second) || scm_is_null (second), second,
                   SCM_ARG4, __FUNCTION__, "Stencil, #f or ()");
  LY_ASSERT_TYPE (is_axis, axis, 2);
  LY_ASSERT_TYPE (is_direction, direction, 3);

  Real p = 0.0;
  if (!SCM_UNBNDP (padding))
    {
      LY_ASSERT_TYPE (scm_is_number, padding, 5);
      p = scm_to_double (padding);
    }

  if (s1)
    result = *s1;

  if (s2)
    result.add_at_edge (Axis (scm_to_int (axis)),
                        Direction (scm_to_int (direction)), *s2, p);

  scm_remember_upto_here_2 (first, second);

  return result.smobbed_copy ();
}

LY_DEFINE (ly_stencil_stack, "ly:stencil-stack", 4, 2, 0,
           (SCM first, SCM axis, SCM direction, SCM second, SCM padding,
            SCM mindist),
           "Construct a stencil by stacking @var{second} next to @var{first}."
           "  @var{axis} can be 0 (x-axis) or@tie{}1 (y-axis)."
           "  @var{direction} can be -1 (left or down) or@tie{}1 (right or"
           " up).  The stencils are juxtaposed with @var{padding} as extra"
           " space.  @var{first} and @var{second} may also be @code{'()} or"
           " @code{#f}.  As opposed to @code{ly:stencil-combine-at-edge},"
           " metrics are suited for successively accumulating lines of"
           " stencils.  Also, @var{second} stencil is drawn last.\n\n"
           "If @var{mindist} is specified, reference points are placed"
           " apart at least by this distance.  If either of the stencils"
           " is spacing, @var{padding} and @var{mindist} do not apply.")
{
  Stencil *s1 = unsmob<Stencil> (first);
  Stencil *s2 = unsmob<Stencil> (second);
  Stencil result;

  SCM_ASSERT_TYPE (s1 || scm_is_false (first) || scm_is_null (first), first,
                   SCM_ARG1, __FUNCTION__, "Stencil, #f or ()");
  SCM_ASSERT_TYPE (s2 || scm_is_false (second) || scm_is_null (second), second,
                   SCM_ARG4, __FUNCTION__, "Stencil, #f or ()");
  LY_ASSERT_TYPE (is_axis, axis, 2);
  LY_ASSERT_TYPE (is_direction, direction, 3);

  Real p = 0.0;
  if (!SCM_UNBNDP (padding))
    {
      LY_ASSERT_TYPE (scm_is_number, padding, 5);
      p = scm_to_double (padding);
    }
  Real d = -infinity_f;
  if (!SCM_UNBNDP (mindist))
    {
      LY_ASSERT_TYPE (scm_is_number, mindist, 6);
      d = scm_to_double (mindist);
    }

  if (s1)
    result = *s1;

  if (s2)
    result.stack (Axis (scm_to_int (axis)), Direction (scm_to_int (direction)),
                  *s2, p, d);

  scm_remember_upto_here_2 (first, second);

  return result.smobbed_copy ();
}

LY_DEFINE (ly_stencil_add, "ly:stencil-add", 0, 0, 1, (SCM args),
           "Combine stencils.  Takes any number of arguments.")
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_REST_ARGUMENT (args);

  SCM expr = SCM_EOL;
  SCM cs = ly_symbol2scm ("combine-stencil");

  Box extent;
  extent.set_empty ();

  while (!SCM_NULLP (args))
    {
      Stencil *s = unsmob<Stencil> (scm_car (args));
      if (!s)
        SCM_ASSERT_TYPE (s, scm_car (args), SCM_ARGn, __FUNCTION__, "Stencil");

      extent.unite (s->extent_box ());
      if (scm_is_pair (s->expr ()) && scm_is_eq (cs, s->expr ()))
        {
          expr = scm_reverse_x (scm_list_copy (scm_cdr (s->expr ())), expr);
        }
      else
        expr = scm_cons (s->expr (), expr);

      args = scm_cdr (args);
    }

  expr = scm_cons (cs, scm_reverse_x (expr, SCM_EOL));
  return Stencil (extent, expr).smobbed_copy ();
}

LY_DEFINE (ly_make_stencil, "ly:make-stencil", 1, 2, 0,
           (SCM expr, SCM xext, SCM yext),
           "Stencils are device independent output expressions."
           "  They carry two pieces of information:\n"
           "\n"
           "@enumerate\n"
           "@item\n"
           "A specification of how to print this object."
           "  This specification is processed by the output backends,"
           " for example @file{scm/output-ps.scm}.\n"
           "\n"
           "@item\n"
           "The vertical and horizontal extents of the object, given as"
           " pairs.  If an extent is unspecified (or if you use"
           " @code{empty-interval} as its value), it is taken to be empty.\n"
           "@end enumerate\n")
{
  SCM_ASSERT_TYPE (!scm_is_pair (expr) || is_stencil_head (scm_car (expr)),
                   expr, SCM_ARG1, __FUNCTION__,
                   "registered stencil expression");

  Interval x;
  if (!SCM_UNBNDP (xext))
    {
      LY_ASSERT_TYPE (is_number_pair, xext, 2);
      x = ly_scm2interval (xext);
    }

  Interval y;
  if (!SCM_UNBNDP (yext))
    {
      LY_ASSERT_TYPE (is_number_pair, yext, 3);
      y = ly_scm2interval (yext);
    }

  Box b (x, y);
  Stencil s (b, expr);
  return s.smobbed_copy ();
}

LY_DEFINE (ly_stencil_aligned_to, "ly:stencil-aligned-to", 3, 0, 0,
           (SCM stil, SCM axis, SCM dir),
           "Align @var{stil} using its own extents.  @var{dir} is a number."
           "  @w{@code{-1}} and @code{1} are left and right, respectively."
           "  Other values are interpolated (so @code{0} means the center).")
{
  LY_ASSERT_SMOB (Stencil, stil, 1);
  LY_ASSERT_TYPE (is_axis, axis, 2);
  LY_ASSERT_TYPE (scm_is_number, dir, 3);

  Stencil target = *unsmob<Stencil> (stil);

  target.align_to ((Axis)scm_to_int (axis), scm_to_double (dir));
  return target.smobbed_copy ();
}

LY_DEFINE (ly_stencil_fonts, "ly:stencil-fonts", 1, 0, 0, (SCM s),
           "Analyze @var{s}, and return a list of fonts used"
           " in@tie{}@var{s}.")
{
  LY_ASSERT_SMOB (Stencil, s, 1);
  Stencil *stil = unsmob<Stencil> (s);
  return find_expression_fonts (stil->expr ());
}

LY_DEFINE (ly_stencil_in_color, "ly:stencil-in-color", 4, 0, 0,
           (SCM stc, SCM r, SCM g, SCM b),
           "Put @var{stc} in a different color.")
{
  LY_ASSERT_SMOB (Stencil, stc, 1);
  Stencil *stil = unsmob<Stencil> (stc);
  return Stencil (stil->extent_box (),
                  scm_list_3 (ly_symbol2scm ("color"), scm_list_3 (r, g, b),
                              stil->expr ()))
      .smobbed_copy ();
}

struct Stencil_interpret_arguments
{
  SCM func;
  SCM arg1;
};

SCM
stencil_interpret_in_scm (void *p, SCM expr)
{
  Stencil_interpret_arguments *ap = (Stencil_interpret_arguments *)p;
  return scm_call_2 (ap->func, ap->arg1, expr);
}

LY_DEFINE (ly_interpret_stencil_expression, "ly:interpret-stencil-expression",
           4, 0, 0, (SCM expr, SCM func, SCM arg1, SCM offset),
           "Parse @var{expr}, feed bits to @var{func} with first arg"
           " @var{arg1} having offset @var{offset}.")
{
  LY_ASSERT_TYPE (ly_is_procedure, func, 2);

  Stencil_interpret_arguments a;
  a.func = func;
  a.arg1 = arg1;
  Offset o = ly_scm2offset (offset);

  interpret_stencil_expression (expr, stencil_interpret_in_scm, (void *)&a, o);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_bracket, "ly:bracket", 4, 0, 0, (SCM a, SCM iv, SCM t, SCM p),
           "Make a bracket in direction@tie{}@var{a}.  The extent of the"
           " bracket is given by @var{iv}.  The wings protrude by an amount"
           " of@tie{}@var{p}, which may be negative.  The thickness is given"
           " by@tie{}@var{t}.")
{
  LY_ASSERT_TYPE (is_axis, a, 1);
  LY_ASSERT_TYPE (is_number_pair, iv, 2);
  LY_ASSERT_TYPE (scm_is_number, t, 3);
  LY_ASSERT_TYPE (scm_is_number, p, 4);

  return Lookup::bracket ((Axis)scm_to_int (a), ly_scm2interval (iv),
                          scm_to_double (t), scm_to_double (p),
                          0.95 * scm_to_double (t))
      .smobbed_copy ();
}

LY_DEFINE (ly_stencil_rotate, "ly:stencil-rotate", 4, 0, 0,
           (SCM stil, SCM angle, SCM x, SCM y),
           "Return a stencil @var{stil} rotated @var{angle} degrees around"
           " the relative offset (@var{x}, @var{y}).  E.g., an offset of"
           " (-1, 1) will rotate the stencil around the left upper corner.")
{
  Stencil *s = unsmob<Stencil> (stil);
  LY_ASSERT_SMOB (Stencil, stil, 1);
  LY_ASSERT_TYPE (scm_is_number, angle, 2);
  LY_ASSERT_TYPE (scm_is_number, x, 3);
  LY_ASSERT_TYPE (scm_is_number, y, 4);
  Real a = scm_to_double (angle);
  Real x_off = scm_to_double (x);
  Real y_off = scm_to_double (y);

  SCM new_s = s->smobbed_copy ();
  Stencil *q = unsmob<Stencil> (new_s);
  q->rotate_degrees (a, Offset (x_off, y_off));
  return new_s;
}

LY_DEFINE (ly_stencil_rotate_absolute, "ly:stencil-rotate-absolute", 4, 0, 0,
           (SCM stil, SCM angle, SCM x, SCM y),
           "Return a stencil @var{stil} rotated @var{angle} degrees around"
           " point (@var{x}, @var{y}), given in absolute coordinates.")
{
  Stencil *s = unsmob<Stencil> (stil);
  LY_ASSERT_SMOB (Stencil, stil, 1);
  LY_ASSERT_TYPE (scm_is_number, angle, 2);
  LY_ASSERT_TYPE (scm_is_number, x, 3);
  LY_ASSERT_TYPE (scm_is_number, y, 4);
  Real a = scm_to_double (angle);
  Real x_off = scm_to_double (x);
  Real y_off = scm_to_double (y);

  SCM new_s = s->smobbed_copy ();
  Stencil *q = unsmob<Stencil> (new_s);
  q->rotate_degrees_absolute (a, Offset (x_off, y_off));
  return new_s;
}

LY_DEFINE (ly_round_filled_box, "ly:round-filled-box", 3, 0, 0,
           (SCM xext, SCM yext, SCM blot),
           "Make a @code{Stencil} object that prints a black box of"
           " dimensions @var{xext}, @var{yext} and roundness @var{blot}.")
{
  LY_ASSERT_TYPE (is_number_pair, xext, 1);
  LY_ASSERT_TYPE (is_number_pair, yext, 2);
  LY_ASSERT_TYPE (scm_is_number, blot, 3);

  return Lookup::round_filled_box (
             Box (ly_scm2interval (xext), ly_scm2interval (yext)),
             scm_to_double (blot))
      .smobbed_copy ();
}

LY_DEFINE (ly_round_filled_polygon, "ly:round-filled-polygon", 2, 1, 0,
           (SCM points, SCM blot, SCM extroversion),
           "Make a @code{Stencil} object that prints a black polygon with"
           " corners at the points defined by @var{points} (list of coordinate"
           " pairs) and roundness @var{blot}.  Optional"
           "@var{extroversion} shifts the outline outward, with the"
           "default of@tie{}@code{-1.0} keeping the outer boundary of"
           "the outline just inside of the polygon.")
{
  SCM_ASSERT_TYPE (scm_ilength (points) > 0, points, SCM_ARG1, __FUNCTION__,
                   "list of coordinate pairs");
  LY_ASSERT_TYPE (scm_is_number, blot, 2);
  Real ext = -1;
  if (!SCM_UNBNDP (extroversion))
    {
      LY_ASSERT_TYPE (scm_is_number, extroversion, 3);
      ext = scm_to_double (extroversion);
    }
  vector<Offset> pts;
  for (SCM p = points; scm_is_pair (p); p = scm_cdr (p))
    {
      SCM scm_pt = scm_car (p);
      if (scm_is_pair (scm_pt))
        {
          pts.push_back (ly_scm2offset (scm_pt));
        }
      else
        {
          // TODO: Print out warning
        }
    }
  return Lookup::round_filled_polygon (pts, scm_to_double (blot), ext)
      .smobbed_copy ();
}

LY_DEFINE (ly_register_stencil_expression, "ly:register-stencil-expression", 1,
           0, 0, (SCM symbol),
           "Add @var{symbol} as head of a stencil expression.")
{
  LY_ASSERT_TYPE (ly_is_symbol, symbol, 1);
  register_stencil_head (symbol);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_all_stencil_expressions, "ly:all-stencil-expressions", 0, 0, 0,
           (), "Return all symbols recognized as stencil expressions.")
{
  return all_stencil_heads ();
}

LY_DEFINE (ly_stencil_scale, "ly:stencil-scale", 3, 0, 0,
           (SCM stil, SCM x, SCM y),
           "Scale stencil @var{stil} using the horizontal and vertical"
           " scaling factors @var{x} and @var{y}.  Negative values will"
           " flip or mirror @var{stil} without changing its origin;"
           " this may result in collisions unless it is repositioned.")
{
  Stencil *s = unsmob<Stencil> (stil);
  LY_ASSERT_SMOB (Stencil, stil, 1);
  LY_ASSERT_TYPE (scm_is_number, x, 2);
  LY_ASSERT_TYPE (scm_is_number, y, 3);

  SCM new_s = s->smobbed_copy ();
  Stencil *q = unsmob<Stencil> (new_s);

  q->scale (scm_to_double (x), scm_to_double (y));
  return new_s;
}

LY_DEFINE (ly_stencil_outline, "ly:stencil-outline", 2, 0, 0,
           (SCM stil, SCM outline),
           "Return a stencil with the stencil expression (inking)"
           " of stencil @var{stil} but with outline and dimensions"
           " from stencil @var{outline}.")
{
  Stencil s = *LY_ASSERT_SMOB (Stencil, stil, 1);
  Stencil o = *LY_ASSERT_SMOB (Stencil, outline, 2);
  return s.with_outline (o).smobbed_copy ();
}
