/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>


#include "spanner.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "tie.hh"
#include "rhythmic-head.hh"
#include "bezier.hh"
#include "paper-column.hh"
#include "warn.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "stencil.hh"
#include "bezier.hh"
#include "stem.hh"
#include "note-head.hh"
#include "tie-column.hh"

/*
  tie: Connect two noteheads.

  What if we have

  c4 ~ \clef bass ; c4 or

  c4 \staffchange c4

  do we have non-horizontal ties then?
  */


void
Tie::set_head (Grob*me,Direction d, Grob * h)
{
  assert (!head (me,d));
  index_set_cell (me->get_property ("head-pair"), d, h->self_scm ());
  
  dynamic_cast<Spanner*> (me)->set_bound (d, h);
  me->add_dependency (h);
}

void
Tie::set_interface (Grob*me)
{
  me->set_property ("head-pair", scm_cons (SCM_EOL, SCM_EOL));
}


Grob*
Tie::head (Grob*me, Direction d) 
{
  SCM c = me->get_property ("head-pair");

  if (scm_is_pair (c))
    return unsmob_grob (index_get_cell (c, d));
  else
    return 0;
}

int
Tie::get_column_rank (Grob *me, Direction d)
{
  Spanner *span = dynamic_cast<Spanner*> (me); 
  Grob * h = head (me, d);
  if (!h)
    h = span->get_bound (d);

  Grob *col = dynamic_cast<Item*> (h)->get_column ();
  return Paper_column::get_rank (col);
}

Real
Tie::get_position (Grob*me) 
{
  Direction d = head (me,LEFT) ? LEFT:RIGHT;
  return Staff_symbol_referencer::get_position (head (me,d));
}


/*
  Default:  Put the tie oppositie of the stem [Wanske p231]

  In case of chords: Tie_column takes over
  
  The direction of the Tie is more complicated (See [Ross] p136 and
  further).

  (what about linebreaks? )  
  
*/
Direction
Tie::get_default_dir (Grob*me) 
{
  Item * sl =  head (me,LEFT) ? Rhythmic_head::get_stem (head (me,LEFT)) :0;
  Item * sr =  head (me,RIGHT) ? Rhythmic_head::get_stem (head (me,RIGHT)) :0;  
  if (sl && sr)
    {
      if (get_grob_direction (sl) == UP
	  && get_grob_direction (sr) == UP)
	return DOWN;
    }
  else if (sl || sr)
    {
      Item *s = sl ? sl : sr;
      return - get_grob_direction (s);
    }

  
  return UP;
}


void
Tie::set_direction (Grob*me)
{
  if (!get_grob_direction (me))
    {
      if (Tie_column::has_interface (me->get_parent (Y_AXIS)))
	Tie_column::set_directions (me->get_parent (Y_AXIS));
      else
	set_grob_direction (me, Tie::get_default_dir (me));
    }
}

/*
  TODO: we should also use thickness for computing the clearance
  between head and tie. Very thick ties will now touch the note head.
  
  */
SCM
Tie::get_control_points (SCM smob)
{  
  Spanner*me = unsmob_spanner (smob);
  Direction headdir = CENTER; 
  if (head (me,LEFT))
    headdir = LEFT;
  else if (head (me,RIGHT))
    headdir = RIGHT;
  else
    {
      programming_error ("Tie without heads.");
      me->suicide ();
      return SCM_EOL;
    }

  set_direction (me);

  Direction dir = get_grob_direction (me);
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Real x_gap_f = robust_scm2double (me->get_property ("x-gap"), 0);

  Grob* l = me->get_bound (LEFT);
  Grob* r = me->get_bound (RIGHT);  

  Grob* commonx = me->common_refpoint (l, X_AXIS);
  commonx = me->common_refpoint (r, X_AXIS);
  
  Real left_x;

  /*
     the tie has to be long enough to be visible, but should not go
    through key sigs. In the 1.5 series the pref.matter - note
    distance is fixed , so this won't be a problem anymore.
   */
  Real lambda = 0.9;		
  
  if (Note_head::has_interface (l))
    {
      Real where = RIGHT;

      /*
	This correction is due te the shape of the black note head.
       */
      if (Rhythmic_head::duration_log (l) == 2)
	where += dir* 0.2;
      left_x = l->extent (l, X_AXIS).linear_combination (where)
	+ x_gap_f;
    }
  else
    left_x = l->extent (l, X_AXIS).linear_combination (lambda);
  

  Real width;
  if (Note_head::has_interface (l) && Note_head::has_interface (r))
    {
      width = 
	+ r->extent (commonx,X_AXIS)[LEFT]
	- l->extent (commonx, X_AXIS)[RIGHT]
	-2 * x_gap_f;
    }
  else
    {
      if (Note_head::has_interface (l))
	width = r->relative_coordinate (commonx, X_AXIS)
	  - l->extent (commonx, X_AXIS)[RIGHT]
	  - 2 * x_gap_f;
      else
	width =
	  - l->extent (commonx, X_AXIS).linear_combination (lambda)  
	  + r->extent (commonx, X_AXIS)[LEFT]
	  - 2 * x_gap_f;
    }
  


  SCM details = me->get_property ("details");

  SCM lim // groetjes aan de chirurgendochter.
    = scm_assq (ly_symbol2scm ("height-limit"),details);
  
  Real h_inf = scm_to_double (scm_cdr (lim)) *  staff_space;
  Real r_0 = scm_to_double (scm_cdr (scm_assq (ly_symbol2scm ("ratio"),details)));

  Bezier b  = slur_shape (width, h_inf, r_0);
  
  /*
    I think this better, particularly for small ties. It always allows the user to move ties if
    they seem in the wrong place

    TODO: what if 2 heads have different size.

  */

  Real ypos = Tie::get_position (me) * staff_space/2
    + dir * scm_to_double (me->get_property ("y-offset"));;

  /*
    Make sure we don't start on a dots
   */
  if (Note_head::has_interface (l) && Rhythmic_head::get_dots (l))
    {
      Grob* dots = Rhythmic_head::get_dots (l);
      if (fabs (staff_space * Staff_symbol_referencer::get_position (dots) /2
	       - ypos) < 0.5)
	{
	  ypos += 0.5 * dir ;
	}
    }

  
  /*
    todo: prevent ending / staffline collision.

    todo: tie / stem collision
   */

  b = slur_shape (width,h_inf, r_0);
  b.scale (1, dir);
  b.translate (Offset (left_x, ypos));
  

  /*
    Avoid colliding of the horizontal part with stafflines.

    
    TODO: redo this, heuristic is half-baken, and ties often look ugly
    as a result.

    TODO: doesn't work when on staff with even number of lines.
   */
  Array<Real> horizontal (b.solve_derivative (Offset (1,0)));
  if (horizontal.size ())
    {
      /*
	ugh. Doesnt work for non-horizontal curves.
       */
      Real y = b.curve_point (horizontal[0])[Y_AXIS];

      Real ry = rint (y/staff_space) * staff_space;
      Real diff = ry - y;
      Real newy = y;

      Real clear = staff_space * scm_to_double (me->get_property ("staffline-clearance"));

      if (fabs (y) <=
	  Staff_symbol_referencer::staff_radius (me) * staff_space + clear
	  && fabs (diff) < clear)
	{
	  Real y1 = ry + clear;
	  Real y2 = ry - clear;

	  /*
	    ugh, we shove the 0.5 out of our sleeves.

	    Any way. This test is to make sure that staffline
	    collision avoidance does not result in completely flat
	    ties.
	   */
	  if (fabs (y1 - ypos) < 0.5)
	    y1 = y2;
	  else if (fabs (y2 - ypos) < 0.5)
	    y2 = y1;
	  
	  newy = (fabs (y1 - y) < fabs (y2 - y)) ? y1 : y2;
	  
	  // newy = ry - 0.5 * staff_space * sign (diff) ;

	  /*
	    we don't want horizontal ties
	   */
	  if (fabs (newy - b.control_[0][Y_AXIS]) < 1e-2)
	    {
	      newy = newy + dir * staff_space; 
	    }
	}

      Real y0 = b.control_ [0][Y_AXIS];
      b.control_[2][Y_AXIS] = 
      b.control_[1][Y_AXIS] =
 (b.control_[1][Y_AXIS] - y0)  * ((newy - y0) / (y - y0)) + y0; 
    }
  else
    programming_error ("Tie is nowhere horizontal");



  SCM controls = SCM_EOL;
  for (int i= 4; i--;)
    controls = scm_cons (ly_offset2scm (b.control_[i]), controls);
  return controls;
}


MAKE_SCHEME_CALLBACK (Tie,print,1);
SCM
Tie::print (SCM smob) 
{
  Grob*me = unsmob_grob (smob);

  SCM cp = me->get_property ("control-points");
  if (!scm_is_pair (cp))		// list is more accurate
    {
      cp = get_control_points (smob);
      me->set_property ("control-points", cp);
    }

  if (!scm_is_pair (cp))
    return Stencil ().smobbed_copy ();
  
  Real thick
    = Staff_symbol_referencer::line_thickness (me)
    *  robust_scm2double (me->get_property ("thickness"), 1);

  Bezier b;
  int i = 0;
  for (SCM s= cp; s != SCM_EOL; s = scm_cdr (s))
    {
      b.control_[i] = ly_scm2offset (scm_car (s));
      i++;
    }
  
   Stencil a = Lookup::slur (b, get_grob_direction (me) * thick, thick);
   
   return a.smobbed_copy ();
}



ADD_INTERFACE (Tie,"tie-interface",
	       "A tie connecting two noteheads.\n"
	       ,
  "y-offset staffline-clearance control-points head-pair details thickness x-gap direction minimum-length");
