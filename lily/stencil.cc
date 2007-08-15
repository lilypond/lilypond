/*
  stencil.cc -- implement Stencil

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "stencil.hh"

#include "main.hh"
#include "font-metric.hh"
#include "input.hh"
#include "string-convert.hh"
#include "warn.hh"

#include "ly-smobs.icc"

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

int
Stencil::print_smob (SCM, SCM port, scm_print_state *)
{
  scm_puts ("#<Stencil ", port);
  scm_puts (" >", port);
  return 1;
}

SCM
Stencil::mark_smob (SCM smob)
{
  Stencil *s = (Stencil *) SCM_CELL_WORD_1 (smob);
  return s->expr_;
}

IMPLEMENT_SIMPLE_SMOBS (Stencil);
IMPLEMENT_TYPE_P (Stencil, "ly:stencil?");
IMPLEMENT_DEFAULT_EQUAL_P (Stencil);

Interval
Stencil::extent (Axis a) const
{
  return dim_[a];
}

bool
Stencil::is_empty () const
{
  return (expr_ == SCM_EOL
	  || dim_[X_AXIS].is_empty ()
	  || dim_[Y_AXIS].is_empty ());
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
Offset
Stencil::origin () const
{
  return origin_;
}

/*
 * Rotate this stencil around the point [x, y]
 */
void
Stencil::rotate (Real a, Offset off)
{
  const Real x_cen = extent (X_AXIS).center ();
  const Real y_cen = extent (Y_AXIS).center ();

  /*
   * Calculate the center of rotation
   */
  const Real x = x_cen + off[X_AXIS] * x_cen;
  const Real y = y_cen + off[Y_AXIS] * y_cen;

  /*
   * Build scheme expression (processed in stencil-interpret.cc)
   */
  expr_ = scm_list_n (ly_symbol2scm ("rotate-stencil"),
		      scm_list_2 (scm_from_double (a),
		      scm_cons (scm_from_double (x), scm_from_double (y))),
		      expr_, SCM_UNDEFINED);

  /*
   * Calculate the new bounding box
   */
  vector<Offset> pts;
  pts.push_back (Offset (-x_cen, -y_cen));
  pts.push_back (Offset (x_cen, -y_cen));
  pts.push_back (Offset (x_cen, y_cen));
  pts.push_back (Offset (-x_cen, y_cen));

  const Offset rot = complex_exp (Offset (0, a * M_PI / 180.0));
  dim_.set_empty ();
  for (vsize i = 0; i < pts.size (); i++)
    dim_.add_point (pts[i] * rot + Offset (x_cen, y_cen));
}

void
Stencil::translate (Offset o)
{
  Axis a = X_AXIS;
  while (a < NO_AXES)
    {
      if (isinf (o[a])
	  || isnan (o[a])

	  // ugh, hardcoded. 
	  || fabs (o[a]) > 1e6)
	{
	  programming_error (String_convert::form_string ("Improbable offset for stencil: %f staff space", o[a])
			     + "\n"
			     + "Setting to zero.");
	  o[a] = 0.0;
	  if (strict_infinity_checking)
	    scm_misc_error (__FUNCTION__, "Improbable offset.", SCM_EOL);
	}
      incr (a);
    }

  expr_ = scm_list_n (ly_symbol2scm ("translate-stencil"),
		      ly_offset2scm (o),
		      expr_, SCM_UNDEFINED);
  if (!is_empty ())
    dim_.translate (o);
  origin_ += o;
}

void
Stencil::translate_axis (Real x, Axis a)
{
  Offset o (0, 0);
  o[a] = x;
  translate (o);
}

void
Stencil::add_stencil (Stencil const &s)
{
  expr_ = scm_list_3 (ly_symbol2scm ("combine-stencil"), s.expr_, expr_);
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
  if (is_empty ())
    return;

  Interval i (extent (a));
  translate_axis (-i.linear_combination (x), a);
}

/* FIXME: unintuitive naming, you would expect *this to be moved.
   Kept (keeping?) API for compat with add_at_edge ().

   What is PADDING, what is MINIMUM, exactly?  */
Stencil
Stencil::moved_to_edge (Axis a, Direction d, Stencil const &s,
			Real padding, Real minimum) const
{
  Interval my_extent = dim_[a];
  Interval i (s.extent (a));
  Real his_extent;
  if (i.is_empty ())
    {
      programming_error ("Stencil::moved_to_edge: adding empty stencil.");
      his_extent = 0.0;
    }
  else
    his_extent = i[-d];

  Real offset = (my_extent.is_empty () ? 0.0 : my_extent[d] - his_extent)
    + d * padding;

  Stencil toadd (s);
  toadd.translate_axis (offset, a);

  if (minimum > 0 && d * (-origin ()[a] + toadd.origin ()[a]) < minimum)
    toadd.translate_axis (-toadd.origin ()[a]
			  + origin ()[a] + d * minimum, a);

  return toadd;
}

/*  See scheme Function.  */
void
Stencil::add_at_edge (Axis a, Direction d, Stencil const &s, Real padding,
		      Real minimum)
{
  add_stencil (moved_to_edge (a, d, s, padding, minimum));
}

Stencil
Stencil::in_color (Real r, Real g, Real b) const
{
  Stencil new_stencil (extent_box (),
		       scm_list_3 (ly_symbol2scm ("color"),
				   scm_list_3 (scm_from_double (r),
					       scm_from_double (g),
					       scm_from_double (b)),
				   expr ()));
  return new_stencil;
}
