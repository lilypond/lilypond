/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>


#include "spanner.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "tie.hh"
#include "rhythmic-head.hh"
#include "bezier.hh"
#include "paper-column.hh"
#include "debug.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "molecule.hh"
#include "bezier-bow.hh"
#include "stem.hh"
#include "note-head.hh"

/*
  tie: Connect two noteheads.

  What if we have

  c4 ~ \clef bass ; c4 or

  c4 \staffchange c4

  do we have non-horizontal ties then?
  */


void
Tie::set_head (Grob*me,Direction d, Item * head_l)
{
  assert (!head (me,d));
  index_set_cell (me->get_grob_property ("heads"), d, head_l->self_scm ());
  
  dynamic_cast<Spanner*> (me)->set_bound (d, head_l);
  me->add_dependency (head_l);
}

void
Tie::set_interface (Grob*me)
{
  me->set_grob_property ("heads", gh_cons (SCM_EOL, SCM_EOL));
  me->set_interface (ly_symbol2scm ("tie-interface"));
}

bool
Tie::has_interface (Grob*me)
{
  return me->has_interface (ly_symbol2scm ("tie-interface"));
}

Grob*
Tie::head (Grob*me, Direction d) 
{
  SCM c = me->get_grob_property ("heads");
  c = index_cell (c, d);

  return unsmob_grob (c);
}

Real
Tie::position_f (Grob*me) 
{
  Direction d = head (me,LEFT) ? LEFT:RIGHT;
  return Staff_symbol_referencer::position_f (head (me,d));
}


/*
  Default:  Put the tie oppositie of the stem [Wanske p231]

  In case of chords: Tie_column takes over
  
  The direction of the Tie is more complicated (See [Ross] p136 and
  further).
*/
Direction
Tie::get_default_dir (Grob*me) 
{
  Item * sl =  head (me,LEFT) ? Rhythmic_head::stem_l (head (me,LEFT)) :0;
  Item * sr =  head (me,RIGHT) ? Rhythmic_head::stem_l (head (me,RIGHT)) :0;  

  if (sl && sr)
    {
      if (Directional_element_interface::get (sl) == UP
	  && Directional_element_interface::get (sr) == UP)
	return DOWN;
    }
  else if (sl || sr)
    {
      Item *s = sl ? sl : sr;
      return - Directional_element_interface::get (s);
    }

  
  return UP;
}


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
      return SCM_UNSPECIFIED;
    }
  
  if (!Directional_element_interface::get (me))
    Directional_element_interface::set (me, Tie::get_default_dir (me));
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Real x_gap_f = gh_scm2double (me->get_grob_property ("x-gap"));

  Grob* l = me->get_bound (LEFT);
  Grob* r = me->get_bound (RIGHT);  

  Grob* commonx = me->common_refpoint (l, X_AXIS);
  commonx = me->common_refpoint (r, X_AXIS);
  
  Real left_x;

  /*
    this is a kludge: the tie has to be long enough to be
    visible, but should not go through key sigs.

 (please fixme)
   */
  Real lambda = 0.5;		
  
  if (Note_head::has_interface (l))
    left_x = l->extent (l, X_AXIS)[RIGHT] + x_gap_f;
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
  
  Direction dir = Directional_element_interface::get (me);

  SCM details = me->get_grob_property ("details");

  SCM lim // groetjes aan de chirurgendochter.
    = scm_assq (ly_symbol2scm ("height-limit"),details);
  
  Real h_inf = gh_scm2double (ly_cdr (lim)) *  staff_space;
  Real r_0 = gh_scm2double (ly_cdr (scm_assq (ly_symbol2scm ("ratio"),details)));

  Bezier b  = slur_shape (width, h_inf, r_0);
  
  /*
    I think this better, particularly for small ties. It always allows the user to move ties if
    they seem in the wrong place

    TODO: what if 2 heads have different size.

  */

  Real ypos = Tie::position_f (me) * staff_space/2
    + dir * gh_scm2double (me->get_grob_property ("y-offset"));;

  /*
    Make sure we don't start on a dots
   */
  if (Note_head::has_interface (l) && Rhythmic_head::dots_l (l))
    {
      Grob* dots = Rhythmic_head::dots_l(l);
      if(fabs (staff_space * Staff_symbol_referencer::position_f (dots) /2
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

      Real clear = staff_space * gh_scm2double (me->get_grob_property ("staffline-clearance"));

      if (fabs (y) <= Staff_symbol_referencer::staff_radius (me)
	  && fabs (diff) < clear)
	{
	  Real y1 = ry + clear;
	  Real y2 = ry - clear;
	  
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
    controls = gh_cons (ly_offset2scm (b.control_[i]), controls);
  return controls;
}


MAKE_SCHEME_CALLBACK (Tie,brew_molecule,1);
SCM
Tie::brew_molecule (SCM smob) 
{
  Grob*me = unsmob_grob (smob);

  SCM cp = me->get_grob_property ("control-points");
  if (cp == SCM_EOL)
    {
      cp = get_control_points (smob);
      me->set_grob_property ("control-points", cp);
    }
  
  Real thick =
    gh_scm2double (me->get_grob_property ("thickness"))
    * me->paper_l ()->get_var ("stafflinethickness");

  Bezier b;
  int i = 0;
  for (SCM s= cp; s != SCM_EOL; s = ly_cdr (s))
    {
      b.control_[i] = ly_scm2offset (ly_car (s));
      i++;
    }
  
   Molecule a = Lookup::slur (b, Directional_element_interface::get (me) * thick, thick);
   
   return a.smobbed_copy ();
}


