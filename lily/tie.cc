/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>

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

void
Tie::set_head (Score_element*me,Direction d, Item * head_l)
{
  assert (!head (me,d));
  index_set_cell (me->get_elt_property ("heads"), d, head_l->self_scm_);
  
  dynamic_cast<Spanner*> (me)->set_bound (d, head_l);
  me->add_dependency (head_l);
}

Tie::Tie(SCM s)
  : Spanner (s)
{
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
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
  ugh: direction of the Tie is more complicated.  See [Ross] p136 and further
 */
Direction
Tie::get_default_dir (Score_element*me) 
{
  Item * sl =  head(me,LEFT) ? Rhythmic_head::stem_l (head (me,LEFT)) :0;
  Item * sr =  head(me,RIGHT) ? Rhythmic_head::stem_l (head (me,RIGHT)) :0;  

  if (sl && Directional_element_interface (sl).get () == UP
      && sr && Directional_element_interface (sr).get () == UP)
    return DOWN;
  else
    return UP;
}



MAKE_SCHEME_CALLBACK(Tie,after_line_breaking);
SCM
Tie::after_line_breaking (SCM smob)
{
  Tie*me = dynamic_cast<Tie*> (unsmob_element (smob));
  
  if (!head (me,LEFT) && !head (me,RIGHT))
    {
      programming_error ("Tie without heads.");
      me->suicide ();
      return SCM_UNDEFINED;
    }

  if (!Directional_element_interface (me).get ())
    Directional_element_interface (me).set (Tie::get_default_dir (me));
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real half_space = staff_space / 2;
  Real x_gap_f = me->paper_l ()->get_var ("tie_x_gap");
  Real y_gap_f = me->paper_l ()->get_var ("tie_y_gap");

  /* 
   Slur and tie placement [OSU]

   Ties:

       * x = inner vertical tangent - d * gap

   */


  /*
    OSU: not different for outer notes, so why all me code?
    ie,  can we drop me, or should it be made switchable.
   */
  if (head (me,LEFT))
    me->dx_f_drul_[LEFT] = Tie::head (me,LEFT)->extent (X_AXIS).length ();
  else
    me->dx_f_drul_[LEFT] = dynamic_cast<Spanner*>(me)->get_broken_left_end_align ();
  me->dx_f_drul_[LEFT] += x_gap_f;
  me->dx_f_drul_[RIGHT] -= x_gap_f;

  /* 
   Slur and tie placement [OSU]  -- check me

   Ties:

       * y = dx <  5ss: horizontal tangent
	 y = dx >= 5ss: y next interline - d * 0.25 ss

	 which probably means that OSU assumes that

	    dy <= 5 dx

	 for smal slurs
   */


  Real ypos = Tie::position_f (me);

  Real y_f = half_space * ypos; 
  int ypos_i = int (ypos);
 
  Real dx_f = me->extent (X_AXIS).length () + me->dx_f_drul_[RIGHT] - me->dx_f_drul_[LEFT];
  Direction dir = Directional_element_interface (me).get();
  if (dx_f < me->paper_l ()->get_var ("tie_staffspace_length"))
    {
      if (abs (ypos_i) % 2)
	y_f += dir * half_space;
      y_f += dir * y_gap_f;
    }
  else
    {
      if (! (abs (ypos_i) % 2))
	y_f += dir * half_space;
      y_f += dir * half_space;
      y_f -= dir * y_gap_f;
    }
  
  me->dy_f_drul_[LEFT] = me->dy_f_drul_[RIGHT] = y_f;

  return SCM_UNDEFINED;
}


MAKE_SCHEME_CALLBACK(Tie,set_spacing_rods);
SCM
Tie::set_spacing_rods (SCM smob)  
{
  Score_element*me = unsmob_element (smob);
  Spanner*sp = dynamic_cast<Spanner*> (me);
  Rod r;

  r.item_l_drul_ [LEFT]=sp->get_bound (LEFT);
  r.item_l_drul_ [RIGHT]=sp->get_bound (RIGHT);  
  
  r.distance_f_ = me->paper_l ()->get_var ("tie_x_minimum");
  r.add_to_cols ();
  return SCM_UNDEFINED;
}






MAKE_SCHEME_CALLBACK(Tie,brew_molecule);
SCM
Tie::brew_molecule (SCM smob) 
{
  Score_element*me = unsmob_element (smob);
  Real thick = me->paper_l ()->get_var ("tie_thickness");
  Bezier one = dynamic_cast<Tie*> (me)->get_curve ();

  Molecule a;
  SCM d =  me->get_elt_property ("dashed");
  if (gh_number_p (d))
    a = me->lookup_l ()->dashed_slur (one, thick, gh_scm2int (d));
  else
    a = me->lookup_l ()->slur (one, Directional_element_interface (me).get () * thick, thick);
  
  return a.create_scheme(); 
}



Bezier
Tie::get_curve () const
{
  Score_element*me = (Score_element*)this;
  Direction d (Directional_element_interface (me).get ());
  Bezier_bow b (get_encompass_offset_arr (), d);

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real h_inf = paper_l ()->get_var ("tie_height_limit_factor") * staff_space;
  Real r_0 = paper_l ()->get_var ("tie_ratio");

  b.set_default_bezier (h_inf, r_0);
  Bezier c = b.get_bezier ();

  /* should do me for slurs as well. */
  Array<Real> horizontal (c.solve_derivative (Offset (1,0)));

  if (horizontal.size ())
    {
      /*
	ugh. Doesnt work for non-horizontal curves.
       */
      Real y = c.curve_point (horizontal[0])[Y_AXIS];

      Real ry = rint (y/staff_space) * staff_space;
      Real diff = ry - y;
      Real newy = y;
      if (fabs (y) <= 2.0
	  && fabs (diff) < paper_l ()->get_var ("tie_staffline_clearance"))
	{
	  newy = ry - 0.5 * staff_space * sign (diff) ;
	}

      Real y0 = c.control_ [0][Y_AXIS];
      c.control_[2][Y_AXIS] = 
      c.control_[1][Y_AXIS] =
	(c.control_[1][Y_AXIS] - y0)  * ((newy - y0) / (y - y0)) + y0; 
    }
  else
    programming_error ("Tie is nowhere horizontal");
  return c;
}

Array<Offset>
Tie::get_encompass_offset_arr () const
{
  Array<Offset> offset_arr;
  offset_arr.push (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));
  offset_arr.push (Offset (spanner_length () + dx_f_drul_[RIGHT],
			   dy_f_drul_[RIGHT]));
		      
  return offset_arr;
}


