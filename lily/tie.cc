/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
Tie::set_head (Score_element*me,Direction d, Item * head_l)
{
  assert (!head (me,d));
  index_set_cell (me->get_elt_property ("heads"), d, head_l->self_scm ());
  
  dynamic_cast<Spanner*> (me)->set_bound (d, head_l);
  me->add_dependency (head_l);
}

void
Tie::set_interface (Score_element*me)
{
  me->set_elt_property ("heads", gh_cons (SCM_EOL, SCM_EOL));
  me->set_interface (ly_symbol2scm ("tie-interface"));
}

bool
Tie::has_interface (Score_element*me)
{
  return me->has_interface (ly_symbol2scm ("tie-interface"));
}

Score_element*
Tie::head (Score_element*me, Direction d) 
{
  SCM c = me->get_elt_property ("heads");
  c = index_cell (c, d);

  return unsmob_element (c);
}

Real
Tie::position_f (Score_element*me) 
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
Tie::get_default_dir (Score_element*me) 
{
  Item * sl =  head(me,LEFT) ? Rhythmic_head::stem_l (head (me,LEFT)) :0;
  Item * sr =  head(me,RIGHT) ? Rhythmic_head::stem_l (head (me,RIGHT)) :0;  

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
  Spanner*me = dynamic_cast<Spanner*> (unsmob_element (smob));
  Direction headdir = CENTER; 
  if (head (me,LEFT))
    headdir = LEFT;
  else if (head(me,RIGHT))
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

  Real x_gap_f = gh_scm2double (me->get_elt_property ("x-gap"));

  Score_element* commonx = me->common_refpoint (me->get_bound (LEFT), X_AXIS);
  commonx = me->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  
  Score_element* l = me->get_bound (LEFT);
  Score_element* r = me->get_bound (RIGHT);  

  Real left_x;
  if (Note_head::has_interface (me->get_bound (LEFT)))
    left_x = l->extent (l, X_AXIS)[RIGHT] + x_gap_f;
  else
    left_x = l->extent (l, X_AXIS).length () / 2;

  Real width;
  if (Note_head::has_interface (me->get_bound (LEFT))
      && Note_head::has_interface (me->get_bound (RIGHT)))
    {
      width = 
	+ r->extent (commonx,X_AXIS)[LEFT]
	- l->extent (commonx, X_AXIS)[RIGHT]
	-2 * x_gap_f;
    }
  else
    {
      if (Note_head::has_interface (me->get_bound (LEFT)))
	width = r->relative_coordinate (commonx, X_AXIS)
	  - l->extent (commonx, X_AXIS)[RIGHT]
	  - 2 * x_gap_f;
      else
	width = 
	  - l->extent (l, X_AXIS).length () / 2
	  + r->extent (commonx, X_AXIS)[LEFT]
	  - l->relative_coordinate (commonx, X_AXIS)
	  - 2 * x_gap_f;
    }
  
  Direction dir = Directional_element_interface::get(me);

  SCM details = me->get_elt_property ("details");

  SCM lim // groetjes aan de chirurgendochter.
    = scm_assq (ly_symbol2scm ("height-limit"),details);
  
  Real h_inf = gh_scm2double (gh_cdr (lim)) *  staff_space;
  Real r_0 = gh_scm2double (gh_cdr (scm_assq (ly_symbol2scm ("ratio"),details)));

  Bezier b  = slur_shape (width, h_inf, r_0);
  
  Offset leave_dir = b.control_[1] - b.control_[0];

  Score_element *hed =head (me, headdir);
  Real dx = (hed->extent (hed, X_AXIS).length () + x_gap_f)/2.0;
  Real max_gap = leave_dir[Y_AXIS] * dx / leave_dir[X_AXIS];

  /*
    for small ties (t small) we want to start in the Y-center (so dy = 0), for
    large ties, the tie should appear to come from the center of the
    head, so dy = max_gap

    maybe use a different formula?

    TODO: what if 2 heads have different size.

    TODO: for small ties, it is better to start over the heads
    iso. next to the heads. 
  */
  Real t = (width / staff_space - 5.0);	// ugh.
  Real dy = t > 0 ? max_gap * sqr (t / (1 + t)) : 0.0;

  Real ypos = Tie::position_f (me) * staff_space/2 + dir * dy;

  /*
    todo: prevent ending / staffline collision.

    todo: tie / stem collision
   */

  b = slur_shape(width,h_inf, r_0);
  b.scale (1, dir);
  b.translate (Offset (left_x, ypos));
  

  /*
    Avoid colliding of the horizontal part with stafflines.
    
    should do me for slurs as well.

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

      Real clear = staff_space * gh_scm2double (me->get_elt_property ("staffline-clearance"));

	if (fabs (y) <= Staff_symbol_referencer::staff_radius (me)
	  && fabs (diff) < clear)
	{
	  newy = ry - 0.5 * staff_space * sign (diff) ;
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
    controls = gh_cons ( ly_offset2scm (b.control_[i]), controls);
  return controls;
}

MAKE_SCHEME_CALLBACK(Tie,set_spacing_rods,1);

/*
  TODO: set minimum distances for begin/end of line
 */
SCM
Tie::set_spacing_rods (SCM smob)  
{
  Score_element*me = unsmob_element (smob);
  Spanner*sp = dynamic_cast<Spanner*> (me);
  Rod r;

  r.item_l_drul_ [LEFT]=sp->get_bound (LEFT);
  r.item_l_drul_ [RIGHT]=sp->get_bound (RIGHT);  
  
  r.distance_f_
    = gh_scm2double (me->get_elt_property ("minimum-length"))
    * me->paper_l ()->get_var ("staffspace");
  r.add_to_cols ();
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK(Tie,brew_molecule,1);
SCM
Tie::brew_molecule (SCM smob) 
{
  Score_element*me = unsmob_element (smob);

  SCM cp = me->get_elt_property ("control-points");
  if (cp == SCM_EOL)
    {
      cp = get_control_points (smob);
      me->set_elt_property ("control-points", cp);
    }
  
  Real thick =
    gh_scm2double (me->get_elt_property ("thickness"))
    * me->paper_l ()->get_var ("stafflinethickness");

  Bezier b;
  int i = 0;
  for (SCM s= cp; s != SCM_EOL; s = gh_cdr (s))
    {
      b.control_[i] = ly_scm2offset (gh_car (s));
      i++;
    }
  
   Molecule a = me->lookup_l ()->slur (b, Directional_element_interface::get (me) * thick, thick);
   
   return a.create_scheme ();
}


