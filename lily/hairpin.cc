/*
  hairpin.cc -- implement Hairpin

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-symbol-referencer.hh"
#include "stencil.hh"
#include "line-interface.hh"
#include "hairpin.hh"
#include "spanner.hh"
#include "font-interface.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "warn.hh"
#include "paper-column.hh"
#include "lookup.hh"

MAKE_SCHEME_CALLBACK (Hairpin, print, 1);

SCM
Hairpin::print (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner*> (me);

  SCM s = me->get_property ("grow-direction");
  if (!is_direction (s))
    {
      me->suicide ();
      return SCM_EOL;
    }
  
  Direction grow_dir = to_dir (s);


  /* Ugh, must be same as Text_spanner::print.  */

  /*
    Ugh. property name is not general.
   */
  Real padding = ly_scm2double (me->get_property ("if-text-padding"));
 
  Drul_array<bool> broken;
  Drul_array<Item*> bounds ;
  Direction d = LEFT;
  do
    {
      bounds[d] = spanner->get_bound (d);
      broken[d] = bounds[d]->break_status_dir () != CENTER;
    }
  while (flip (&d) != LEFT);

  Grob *common = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);
  Drul_array<Real> x_points ;
  
  do
    {
      Item *b = bounds[d];
      x_points[d] = b->relative_coordinate (common, X_AXIS);
      if (broken [d])
	{
	  if (d == LEFT)
	    x_points[d] = b->extent (common,X_AXIS)[RIGHT] ;
	}
      else
	{
	  if (dynamic_cast<Paper_column*> (b))
	    {
	      /*
		If we're hung on a paper column, that means we're not
		adjacent to a text-dynamic, and we may move closer. We
		make the padding a little smaller, here.
	      */
	      Interval e = b->extent (common, X_AXIS);
	      if (e.is_empty ())
		e = Interval (0,0) + b->relative_coordinate (common, X_AXIS);
	      
	      x_points[d] = e.center () - d  * padding / 3; // ugh.
	    }
	  else
	    {
	      Interval e = b->extent (common, X_AXIS);
	      if (!e.is_empty ())
		x_points[d] = e[-d] - d*padding;
	    }
	}
    }
  while (flip (&d) != LEFT);


  Real width = x_points[RIGHT] - x_points[LEFT];

  if (width < 0)
    {
      me->warning (_ ((grow_dir < 0) ? "decrescendo too small"
		  : "crescendo too small"));
      width = 0;
    }

  bool continued = broken[Direction (-grow_dir)];
  Real height = robust_scm2double (me->get_property ("height"), 0.2) *
    Staff_symbol_referencer::staff_space (me);

  Real starth, endh;
  if (grow_dir < 0)
    {
      starth = height;
      endh = continued ? height/2 : 0.0;
    }
  else
    {
      starth = continued ? height/2 : 0.0;
      endh = height;
    }

  /*
    should do relative to staff-symbol staff-space?
  */

  Stencil mol;
  mol  = Line_interface::line (me, Offset (0, starth), Offset (width, endh));
  mol.add_stencil (Line_interface::line (me,
					 Offset (0, -starth),
					 Offset (width, -endh)));

  mol.translate_axis (x_points[LEFT]
		      - bounds[LEFT]->relative_coordinate (common, X_AXIS),
		      X_AXIS);
  return mol.smobbed_copy ();
}



ADD_INTERFACE (Hairpin, "hairpin-interface",
  "A hairpin (de)crescendo.",
  "grow-direction height if-text-padding");

