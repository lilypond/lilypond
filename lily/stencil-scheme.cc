/*
  stencil-scheme.cc -- implement Stencil scheme accessors

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/


#include "font-metric.hh"
#include "libc-extension.hh"
#include "lookup.hh"
#include "stencil.hh"

/*
  TODO: naming add/combine.
*/

LY_DEFINE (ly_translate_stencil_axis, "ly:stencil-translate-axis",
	   3, 0, 0, (SCM stil, SCM amount, SCM axis),
	   "Return a copy of @var{stil} but translated by @var{amount} in @var{axis} direction.")
{
  Stencil *s = unsmob_stencil (stil);
  SCM_ASSERT_TYPE (s, stil, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (scm_is_number (amount), amount, SCM_ARG2, __FUNCTION__, "number");

  Real real_amount = scm_to_double (amount);

#if 0
  SCM_ASSERT_TYPE (!isinf (real_amount) && !isnan (real_amount),
		   amount, SCM_ARG2, __FUNCTION__, "finite number");
#endif

  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");

  SCM new_s = s->smobbed_copy ();
  Stencil *q = unsmob_stencil (new_s);
  q->translate_axis (real_amount, Axis (scm_to_int (axis)));
  return new_s;
}

LY_DEFINE (ly_translate_stencil, "ly:stencil-translate",
	   2, 0, 0, (SCM stil, SCM offset),
	   "Return a @var{stil}, "
	   "but translated by @var{offset} (a pair of numbers).")
{
  Stencil *s = unsmob_stencil (stil);
  SCM_ASSERT_TYPE (s, stil, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (is_number_pair (offset), offset, SCM_ARG2, __FUNCTION__, "number pair");
  Offset o = ly_scm2offset (offset);

  SCM new_s = s->smobbed_copy ();
  Stencil *q = unsmob_stencil (new_s);
  q->translate (o);
  return new_s;
}

LY_DEFINE (ly_stencil_expr, "ly:stencil-expr",
	   1, 0, 0, (SCM stil),
	   "Return the expression of @var{stil}.")
{
  Stencil *s = unsmob_stencil (stil);
  SCM_ASSERT_TYPE (s, stil, SCM_ARG1, __FUNCTION__, "stencil");
  return s->expr ();
}

LY_DEFINE (ly_stencil_extent, "ly:stencil-extent",
	   2, 0, 0, (SCM stil, SCM axis),
	   "Return a pair of numbers signifying the extent of @var{stil} in "
	   "@var{axis} direction (0 or 1 for x and y axis respectively).")
{
  Stencil *s = unsmob_stencil (stil);
  SCM_ASSERT_TYPE (s, stil, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");

  return ly_interval2scm (s->extent (Axis (scm_to_int (axis))));
}

LY_DEFINE (ly_stencil_empty_p, "ly:stencil-empty?",
	   1, 0, 0, (SCM stil),
	   "Return whether @var{stil} is empty ")
{
  Stencil *s = unsmob_stencil (stil);
  SCM_ASSERT_TYPE (s, stil, SCM_ARG1, __FUNCTION__, "stencil");
  return scm_from_bool (s->is_empty ());
}

LY_DEFINE (ly_stencil_origin, "ly:stencil-origin",
	   2, 0, 0, (SCM stil, SCM axis),
	   "Return a pair of numbers signifying the origin @var{stil} in "
	   "@var{axis} direction (0 or 1 for x and y axis respectively).")
{
  Stencil *s = unsmob_stencil (stil);
  SCM_ASSERT_TYPE (s, stil, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");

  return scm_from_double (s->origin ()[Axis (scm_to_int (axis))]);
}

LY_DEFINE (ly_stencil_moved_to_edge, "ly:stencil-moved-to-edge",
	   4, 2, 0, (SCM first, SCM axis, SCM direction, SCM second,
		     SCM padding, SCM minimum),
	   "Similar to @code{ly:stencil-combine-edge}, but returns "
	   "@var{second} positioned to be next to @var{first}. ")
{
  /*
    C&P from combine-at-edge.
  */
  Stencil *s1 = unsmob_stencil (first);
  Stencil *s2 = unsmob_stencil (second);
  Stencil first_stencil;

  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE (is_direction (direction), direction, SCM_ARG4, __FUNCTION__, "dir");

  Real p = 0.0;
  if (padding != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_number (padding), padding, SCM_ARG5, __FUNCTION__, "number");
      p = scm_to_double (padding);
    }
  Real m = 0.0;
  if (minimum != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_number (minimum), minimum, SCM_ARG6, __FUNCTION__, "number");
      m = scm_to_double (minimum);
    }

  if (s1)
    first_stencil = *s1;

  if (s2)
    return first_stencil.moved_to_edge (Axis (scm_to_int (axis)),
					Direction (scm_to_int (direction)),
					*s2, p, m).smobbed_copy ();
  else
    return Stencil ().smobbed_copy ();
}

LY_DEFINE (ly_stencil_combine_at_edge, "ly:stencil-combine-at-edge",
	   4, 2, 0, (SCM first, SCM axis, SCM direction,
		     SCM second,
		     SCM padding,
		     SCM minimum),
	   "Construct a stencil by putting @var{second} next to @var{first}. "
	   "@var{axis} can be 0 (x-axis) or 1 (y-axis), "
	   "@var{direction} can be -1 (left or down) or 1 (right or up). "
	   "The stencils are juxtaposed with  @var{padding} as extra space. "
	   "If this puts the reference points closer than @var{minimum}, "
	   "they are moved by the latter amount."
	   "@var{first} and @var{second} may also be '() or #f.")
{
  Stencil *s1 = unsmob_stencil (first);
  Stencil *s2 = unsmob_stencil (second);
  Stencil result;

  SCM_ASSERT_TYPE (s1 || first == SCM_BOOL_F || first == SCM_EOL,
		   first, SCM_ARG1, __FUNCTION__, "Stencil, #f or ()");
  SCM_ASSERT_TYPE (s2 || second == SCM_BOOL_F || second == SCM_EOL,
		   second, SCM_ARG4, __FUNCTION__, "Stencil, #f or ()");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE (is_direction (direction), direction, SCM_ARG3, __FUNCTION__, "dir");

  Real p = 0.0;
  if (padding != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_number (padding), padding, SCM_ARG5, __FUNCTION__, "number");
      p = scm_to_double (padding);
    }
  Real m = 0.0;
  if (minimum != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_number (minimum), minimum, SCM_ARG6, __FUNCTION__, "number");
      m = scm_to_double (minimum);
    }

  if (s1)
    result = *s1;

  if (s2)
    result.add_at_edge (Axis (scm_to_int (axis)),
			Direction (scm_to_int (direction)), *s2, p, m);

  return result.smobbed_copy ();
}

LY_DEFINE (ly_stencil_add, "ly:stencil-add",
	   0, 0, 1, (SCM args),
	   "Combine stencils. Takes any number of arguments.")
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_REST_ARGUMENT (args);

  SCM expr = SCM_EOL;
  SCM *tail = &expr;
  Box extent;
  extent.set_empty ();

  while (!SCM_NULLP (args))
    {
      Stencil *s = unsmob_stencil (scm_car (args));
      if (!s)
	SCM_ASSERT_TYPE (s, scm_car (args), SCM_ARGn, __FUNCTION__, "Stencil");

      extent.unite (s->extent_box ());
      *tail = scm_cons (s->expr (), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
      args = scm_cdr (args);
    }

  expr = scm_cons (ly_symbol2scm ("combine-stencil"), expr);
  return Stencil (extent, expr).smobbed_copy ();
}

LY_DEFINE (ly_make_stencil, "ly:make-stencil",
	   1, 2, 0, (SCM expr, SCM xext, SCM yext),
	   " \n"
	   "Stencils are a device independent output expressions."
	   "They carry two pieces of information: \n\n"
	   "1: a specification of how to print this object. "
	   "This specification is processed by the output backends, "
	   " for example @file{scm/output-ps.scm}.\n\n"
	   "2: the vertical and horizontal extents of the object.\n\n"
	   "If the extents are unspecified, they are taken  to be empty."
	   )
{
  SCM_ASSERT_TYPE (!scm_is_pair (expr)
		   || is_stencil_head (scm_car (expr)),
		   expr, SCM_ARG1, __FUNCTION__, "registered stencil expression");


  Interval x; 
  if (xext != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (is_number_pair (xext), xext, SCM_ARG2, __FUNCTION__, "number pair");
      x = ly_scm2interval (xext);
    }

  Interval y; 
  if (yext != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (is_number_pair (yext), yext, SCM_ARG3, __FUNCTION__, "number pair");
      y = ly_scm2interval (yext);
    }

  Box b (x, y);
  Stencil s (b, expr);
  return s.smobbed_copy ();
}

LY_DEFINE (ly_stencil_aligned_to, "ly:stencil-aligned-to",
	   3, 0, 0, (SCM stil, SCM axis, SCM dir),
	   "Align @var{stil} using its own extents. "
	   "@var{dir} is a number -1, 1 are left and right respectively. "
	   "Other values are interpolated (so 0 means the center).")
{
  SCM_ASSERT_TYPE (unsmob_stencil (stil), stil, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE (scm_is_number (dir), dir, SCM_ARG3, __FUNCTION__, "number");

  Stencil target = *unsmob_stencil (stil);

  target.align_to ((Axis)scm_to_int (axis),
		   scm_to_double (dir));
  return target.smobbed_copy ();
}

LY_DEFINE (ly_stencil_fonts, "ly:stencil-fonts",
	   1, 0, 0, (SCM s),
	   " Analyse @var{s}, and return a list of fonts used in @var{s}.")
{
  Stencil *stil = unsmob_stencil (s);
  SCM_ASSERT_TYPE (stil, s, SCM_ARG1, __FUNCTION__, "Stencil");
  return find_expression_fonts (stil->expr ());
}

LY_DEFINE (ly_stencil_in_color, "ly:stencil-in-color",
	   4, 0, 0, (SCM stc, SCM r, SCM g, SCM b),
	   "Put @var{stc} in a different color.")
{
  Stencil *stil = unsmob_stencil (stc);
  SCM_ASSERT_TYPE (stil, stc, SCM_ARG1, __FUNCTION__, "Stencil");
  return Stencil (stil->extent_box (),
		  scm_list_3 (ly_symbol2scm ("color"),
			      scm_list_3 (r, g, b),
			      stil->expr ())).smobbed_copy ();
}

struct Stencil_interpret_arguments
{
  SCM func;
  SCM arg1;
};

void stencil_interpret_in_scm (void *p, SCM expr)
{
  Stencil_interpret_arguments *ap = (Stencil_interpret_arguments *) p;
  scm_call_2 (ap->func, ap->arg1, expr);
}

LY_DEFINE (ly_interpret_stencil_expression, "ly:interpret-stencil-expression",
	   4, 0, 0, (SCM expr, SCM func, SCM arg1, SCM offset),
	   "Parse EXPR, feed bits to FUNC with first arg ARG1")
{
  SCM_ASSERT_TYPE (ly_is_procedure (func), func, SCM_ARG1, __FUNCTION__,
		   "procedure");

  Stencil_interpret_arguments a;
  a.func = func;
  a.arg1 = arg1;
  Offset o = ly_scm2offset (offset);

  interpret_stencil_expression (expr, stencil_interpret_in_scm, (void *) & a, o);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_bracket, "ly:bracket",
	   4, 0, 0,
	   (SCM a, SCM iv, SCM t, SCM p),
	   "Make a bracket in direction @var{a}. The extent of the bracket is "
	   "given by @var{iv}. The wings protude by an amount of @var{p}, which "
	   "may be negative. The thickness is given by @var{t}.")
{
  SCM_ASSERT_TYPE (is_axis (a), a, SCM_ARG1, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE (is_number_pair (iv), iv, SCM_ARG2, __FUNCTION__, "number pair");
  SCM_ASSERT_TYPE (scm_is_number (t), a, SCM_ARG3, __FUNCTION__, "number");
  SCM_ASSERT_TYPE (scm_is_number (p), a, SCM_ARG4, __FUNCTION__, "number");

  return Lookup::bracket ((Axis)scm_to_int (a), ly_scm2interval (iv),
			  scm_to_double (t),
			  scm_to_double (p),
			  0.95 * scm_to_double (t)).smobbed_copy ();
}

LY_DEFINE (ly_rotate_stencil, "ly:stencil-rotate",
	   4, 0, 0, (SCM stil, SCM angle, SCM x, SCM y),
	   "Return a @var{stil} rotated @var{angle} degrees around point (@var{x}, @var{y}).")
{
  Stencil *s = unsmob_stencil (stil);
  SCM_ASSERT_TYPE (s, stil, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (scm_is_number (angle), angle, SCM_ARG2, __FUNCTION__, "number");
  SCM_ASSERT_TYPE (scm_is_number (x), x, SCM_ARG3, __FUNCTION__, "number");
  SCM_ASSERT_TYPE (scm_is_number (y), y, SCM_ARG4, __FUNCTION__, "number");
  Real a = scm_to_double (angle);
  Real x_off = scm_to_double (x);
  Real y_off = scm_to_double (y);

  SCM new_s = s->smobbed_copy ();
  Stencil *q = unsmob_stencil (new_s);
  q->rotate (a, Offset (x_off, y_off));
  return new_s;
}

LY_DEFINE (ly_filled_box, "ly:round-filled-box",
	   3, 0, 0,
	   (SCM xext, SCM yext, SCM blot),
	   "Make a @code{Stencil} "
	   "that prints a black box of dimensions @var{xext}, "
	   "@var{yext} and roundness @var{blot}.")
{
  SCM_ASSERT_TYPE (is_number_pair (xext), xext, SCM_ARG1, __FUNCTION__, "number pair");
  SCM_ASSERT_TYPE (is_number_pair (yext), yext, SCM_ARG2, __FUNCTION__, "number pair");
  SCM_ASSERT_TYPE (scm_is_number (blot), blot, SCM_ARG3, __FUNCTION__, "number");

  return Lookup::round_filled_box (Box (ly_scm2interval (xext), ly_scm2interval (yext)),
				   scm_to_double (blot)).smobbed_copy ();
}

LY_DEFINE (ly_register_stencil_expression, "ly:register-stencil-expression",
	   1, 0, 0,
	   (SCM symbol),
	   "Add @var{symbol} as head of a stencil expression")
{
  SCM_ASSERT_TYPE (scm_is_symbol (symbol), symbol,
		   SCM_ARG1, __FUNCTION__, "Symbol");
  register_stencil_head (symbol);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_all_stencil_expressions, "ly:all-stencil-expressions",
	   0, 0, 0,
	   (),
	   "Return all symbols recognized as stencil expressions.")
{
  return all_stencil_heads ();
}
