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
Tie::set_head (Direction d, Item * head_l)
{
  assert (!head (d));
  index_set_cell (get_elt_pointer ("heads"), d, head_l->self_scm_);
  
  set_bound (d, head_l);
  add_dependency (head_l);
}

Tie::Tie(SCM s)
  : Spanner (s)
{
  set_elt_pointer ("heads", gh_cons (SCM_EOL, SCM_EOL));
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;

}

Rhythmic_head* 
Tie::head (Direction d) const
{
  SCM c = get_elt_pointer ("heads");
  c = index_cell (c, d);

  return dynamic_cast<Rhythmic_head*> (unsmob_element (c));  
}

Real
Tie::position_f () const
{
  return head (LEFT)
    ? Staff_symbol_referencer_interface (head (LEFT)).position_f ()
    : Staff_symbol_referencer_interface (head (RIGHT)).position_f () ;  
}


/*
  ugh: direction of the Tie is more complicated.  See [Ross] p136 and further
 */
Direction
Tie::get_default_dir () const
{
  Stem * sl =  head(LEFT) ? head (LEFT)->stem_l () :0;
  Stem * sr =  head(RIGHT) ? head (RIGHT)->stem_l () :0;  

  if (sl && Directional_element_interface (sl).get () == UP
      && sr && Directional_element_interface (sr).get () == UP)
    return DOWN;
  else
    return UP;
}

/*
  fixme  must  use spanned drul from heads elt property
 */

void
Tie::do_add_processing()
{
  if (!(head (LEFT) && head (RIGHT)))
    warning (_ ("lonely tie"));

  Direction d = LEFT;
  Drul_array<Rhythmic_head *> new_head_drul;
  new_head_drul[LEFT] = head(LEFT);
  new_head_drul[RIGHT] = head(RIGHT);  
  do {
    if (!head (d))
      new_head_drul[d] = head((Direction)-d);
  } while (flip(&d) != LEFT);

  index_set_cell (get_elt_pointer ("heads"), LEFT, new_head_drul[LEFT]->self_scm_ );
  index_set_cell (get_elt_pointer ("heads"), RIGHT, new_head_drul[RIGHT]->self_scm_ );
}

GLUE_SCORE_ELEMENT(Tie,after_line_breaking);
SCM
Tie::member_after_line_breaking ()
{
  if (!head (LEFT) && !head (RIGHT))
    {
      programming_error ("Tie without heads.");
      suicide ();
      return SCM_UNDEFINED;
    }

  if (!Directional_element_interface (this).get ())
    Directional_element_interface (this).set (get_default_dir ());
  
  Real staff_space = Staff_symbol_referencer_interface (this).staff_space ();
  Real half_space = staff_space / 2;
  Real x_gap_f = paper_l ()->get_var ("tie_x_gap");
  Real y_gap_f = paper_l ()->get_var ("tie_y_gap");

  /* 
   Slur and tie placement [OSU]

   Ties:

       * x = inner vertical tangent - d * gap

   */


  /*
    OSU: not different for outer notes, so why all this code?
    ie,  can we drop this, or should it be made switchable.
   */
  if (head (LEFT))
    dx_f_drul_[LEFT] = head (LEFT)->extent (X_AXIS).length ();
  else
    dx_f_drul_[LEFT] = get_broken_left_end_align ();
  dx_f_drul_[LEFT] += x_gap_f;
  dx_f_drul_[RIGHT] -= x_gap_f;

  /* 
   Slur and tie placement [OSU]  -- check this

   Ties:

       * y = dx <  5ss: horizontal tangent
	 y = dx >= 5ss: y next interline - d * 0.25 ss

	 which probably means that OSU assumes that

	    dy <= 5 dx

	 for smal slurs
   */


  Real ypos = position_f ();

  Real y_f = half_space * ypos; 
  int ypos_i = int (ypos);
 
  Real dx_f = extent (X_AXIS).length () + dx_f_drul_[RIGHT] - dx_f_drul_[LEFT];
  Direction dir = Directional_element_interface (this).get();
  if (dx_f < paper_l ()->get_var ("tie_staffspace_length"))
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
  
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = y_f;

  return SCM_UNDEFINED;
}



Array<Rod>
Tie::get_rods () const
{
  Array<Rod> a;
  Rod r;

  r.item_l_drul_ [LEFT]=get_bound (LEFT);
  r.item_l_drul_ [RIGHT]=get_bound (RIGHT);  
  
  r.distance_f_ = paper_l ()->get_var ("tie_x_minimum");
  a.push (r);
  return a;
}

GLUE_SCORE_ELEMENT(Tie,brew_molecule);

SCM
Tie::member_brew_molecule () const
{
  Real thick = paper_l ()->get_var ("tie_thickness");
  Bezier one = get_curve ();

  Molecule a;
  SCM d =  get_elt_property ("dashed");
  if (gh_number_p (d))
    a = lookup_l ()->dashed_slur (one, thick, gh_scm2int (d));
  else
    a = lookup_l ()->slur (one, Directional_element_interface (this).get () * thick, thick);
  
  return a.create_scheme(); 
}



Bezier
Tie::get_curve () const
{
  Direction d (Directional_element_interface (this).get ());
  Bezier_bow b (get_encompass_offset_arr (), d);

  Real staff_space = Staff_symbol_referencer_interface (this).staff_space ();
  Real h_inf = paper_l ()->get_var ("tie_height_limit_factor") * staff_space;
  Real r_0 = paper_l ()->get_var ("tie_ratio");

  b.set_default_bezier (h_inf, r_0);
  Bezier c = b.get_bezier ();

  /* should do this for slurs as well. */
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


