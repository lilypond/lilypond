/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  [TODO]
    * begin and end should be treated as a/acknowledge Scripts.
    * broken slur should have uniform trend
 */

#include "directional-element-interface.hh"
#include "group-interface.hh"
#include "slur.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "note-column.hh"
#include "stem.hh"
#include "paper-column.hh"
#include "molecule.hh"
#include "debug.hh"
#include "box.hh"
#include "bezier-bow.hh"
#include "main.hh"
#include "cross-staff.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"



class Slur_bezier_bow : public Bezier_bow
{
public:
  Slur_bezier_bow (Array<Offset> encompass, Direction dir);
  Array<Real> area_x_gradients_array (Real area);
  void blow_fit ();
  Real enclosed_area_f () const;
  Real fit_factor () const;
  void minimise_enclosed_area (Paper_def* paper_l, Real default_height);
};

Slur_bezier_bow::Slur_bezier_bow (Array<Offset> encompass, Direction dir)
  : Bezier_bow (encompass, dir)
{
}

void
Slur_bezier_bow::blow_fit ()
{
  Real len = curve_.control_[3][X_AXIS]; 
  Real h = curve_.control_[1][Y_AXIS] * fit_factor () / len;
  curve_.control_[1][Y_AXIS] = h * len;
  curve_.control_[2][Y_AXIS] = h * len;  
  curve_.assert_sanity ();
}


Real
Slur_bezier_bow::enclosed_area_f () const
{
  Real a = 0;
  for (int i=0; i < encompass_.size (); i++)
    {
      Interval x;
      Interval y;
      if (i == 0)
	{
	  x = Interval (0, encompass_[1][X_AXIS] / 2);
	  y = Interval (0,
			curve_.get_other_coordinate (X_AXIS,
						     encompass_[1][X_AXIS]
						     / 2));
	}
      else if (i == encompass_.size () - 1)
	{
	  x = Interval ((encompass_[i-1][X_AXIS] + encompass_[i][X_AXIS])/2, 
			encompass_[i][X_AXIS]);
	  y = Interval (0,
			(curve_.get_other_coordinate (X_AXIS,
						      (x[MIN] + x[MAX]) / 2)));
	}
      else
	{
	  x = Interval ((encompass_[i-1][X_AXIS] + encompass_[i][X_AXIS]) / 2, 
			(encompass_[i][X_AXIS] + encompass_[i+1][X_AXIS]) / 2);
	  y = Interval (encompass_[i][Y_AXIS],
			(curve_.get_other_coordinate (X_AXIS, x[MIN])
			 + curve_.get_other_coordinate (X_AXIS,
							(x[MIN] + x[MAX]) / 2)
			 + curve_.get_other_coordinate (X_AXIS, x[MAX])) / 3);
	}
      
      Real da = x.length () * y.length ();
      a += da;
    }
  return a;
}

Array<Real>
Slur_bezier_bow::area_x_gradients_array (Real area)
{
  Real len = curve_.control_[3][X_AXIS]; 
  Real grow = len / 10.0;
  Array<Real> da (2);
  for (int i=0; i < 2; i++)
    {
      Real r = curve_.control_[i+1][X_AXIS];
      curve_.control_[i+1][X_AXIS] += grow;
      da[i] = (enclosed_area_f () - area) / grow;
      curve_.control_[i+1][X_AXIS] = r; 
    }
  return da;
}

void
Slur_bezier_bow::minimise_enclosed_area (Paper_def* paper_l,
					 Real default_height)
{
  Real length = curve_.control_[3][X_AXIS]; 
  Real sb = paper_l->get_var ("slur_beautiful");
  Real beautiful = length * default_height * sb;

  DEBUG_OUT << to_str ("Beautiful: %f\n", beautiful);
  DEBUG_OUT << to_str ("Length: %f\n", length);
  DEBUG_OUT << to_str ("D-height: %f\n", default_height);
  DEBUG_OUT << to_str ("FitFac: %f\n", fit_factor ());

  if (fit_factor () > 1.0)
    blow_fit ();
  
  Real pct_c0 = paper_l->get_var ("bezier_pct_c0");
  Real pct_c3 = paper_l->get_var ("bezier_pct_c3");
  Real pct_in_max = paper_l->get_var ("bezier_pct_in_max");
  Real pct_out_max = paper_l->get_var ("bezier_pct_out_max");
  Real steps = paper_l->get_var ("bezier_area_steps");

  for (int i=0; i < steps; i++)
    {
      Real area = enclosed_area_f ();
      if (!i)
	DEBUG_OUT << to_str ("Init area: %f\n", area);

      if (area <= beautiful)
	break;

      Array<Real> da = area_x_gradients_array (area);

      // urg
      Real pct = pct_c0 + pct_c3 * length * length * length;
      pct *= (steps - i) / steps;
      if (da[0] > 0 || da[1] < 0)
	pct = pct <? pct_out_max;
      else
	pct = pct <? pct_in_max;

      Real u = (abs (curve_.control_[1][X_AXIS] / da[0])
		<? abs ((curve_.control_[3][X_AXIS]
			 - curve_.control_[2][X_AXIS]) / da[1]));

      DEBUG_OUT << to_str ("pct: %f\n", pct);
      DEBUG_OUT << to_str ("u: %f\n", u);

      DEBUG_OUT << to_str ("da: (%f, %f)\n", da[0], da[1]);
      DEBUG_OUT << to_str ("da*u: (%f, %f)\n", da[0]*u*pct, da[1]*u*pct);
      DEBUG_OUT << to_str ("cx: (%f, %f)\n", curve_.control_[1][X_AXIS],
			   curve_.control_[2][X_AXIS]);

      curve_.control_[1][X_AXIS] -= da[0] * u * pct;
      curve_.control_[2][X_AXIS] -= da[1] * u * pct;
    }

  Real area = enclosed_area_f ();
  DEBUG_OUT << to_str ("Exarea: %f\n", area);
}



/*
  max ( encompass.y / curve.y )
  
 */
Real
Slur_bezier_bow::fit_factor () const
{
  Real x1 = encompass_[0][X_AXIS];
  Real x2 = encompass_.top ()[X_AXIS];

  Real factor = 0.0;
  for (int i=1; i < encompass_.size ()-1; i++)
    {
      if (encompass_[i][X_AXIS] > x1 && encompass_[i][X_AXIS] < x2)
	{
	 Real y = curve_.get_other_coordinate (X_AXIS, encompass_[i][X_AXIS]);
	 if (y>0)
	   {
	     Real f = encompass_[i][Y_AXIS] / y;
	     factor = factor >? f;
	   }
	}
    }


  return factor;
}





/*
  Slur
*/

Slur::Slur ()
{
  // URG
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;

  set_elt_property ("note-columns", SCM_EOL);
  set_elt_property ("control-points", SCM_EOL);

#if 0
  /*
    I still don't understand the merits of this Group_interface.
   */
  Group_interface c (this, "control-points");
  c.set_interface ();
#endif
}

void
Slur::add_column (Note_column*n)
{
  if (!gh_pair_p (n->get_elt_property ("note-heads")))
    warning (_ ("Putting slur over rest.  Ignoring."));
  else
    {
      Group_interface gi (this, "note-columns");
      gi.add_element (n);
      add_dependency (n);
    }
}

void
Slur::de_uglyfy (Slur_bezier_bow* bb, Real default_height)
{
  Real length = bb->curve_.control_[3][X_AXIS] ; 
  Real ff = bb->fit_factor ();
  for (int i = 1; i < 3; i++)
    {
      Real ind = abs (bb->curve_.control_[(i-1)*3][X_AXIS]
		      - bb->curve_.control_[i][X_AXIS]) / length;
      Real h = bb->curve_.control_[i][Y_AXIS] * ff / length;

      Real f = default_height / length;
      Real c1 = paper_l ()->get_var ("bezier_control1");
      Real c2 = paper_l ()->get_var ("bezier_control2");
      Real c3 = paper_l ()->get_var ("bezier_control3");
      if (h > c1 * f)
	{
	  h = c1 * f; 
	}
      else if (h > c2 + c3 * ind)
	{
	  h = c2 + c3 * ind; 
	}
      
      bb->curve_.control_[i][Y_AXIS] = h * length;
    } 

  bb->curve_.assert_sanity ();
}

Direction
Slur::get_default_dir () const
{
  Link_array<Note_column> encompass_arr =
    Group_interface__extract_elements (this, (Note_column*)0, "note-columns");
  
  Direction d = DOWN;
  for (int i=0; i < encompass_arr.size (); i ++) 
    {
      if (encompass_arr[i]->dir () < 0) 
	{
	  d = UP;
	  break;
	}
    }
  return d;
}

void
Slur::do_add_processing ()
{
  Link_array<Note_column> encompass_arr =
    Group_interface__extract_elements (this, (Note_column*)0, "note-columns");

  if (encompass_arr.size ())
    {
      set_bound (LEFT, encompass_arr[0]);    
      if (encompass_arr.size () > 1)
	set_bound (RIGHT, encompass_arr.top ());
    }
}



Offset
Slur::encompass_offset (Note_column const* col) const
{
  Offset o;
  Stem* stem_l = col->stem_l ();
  Direction dir = directional_element (this).get ();
  
  if (!stem_l)
    {
      warning (_ ("Slur over rest?"));
     o[X_AXIS] = col->relative_coordinate (0, X_AXIS);
      o[Y_AXIS] = col->extent (Y_AXIS)[dir];
      return o;  
    }
  Direction stem_dir = directional_element (stem_l).get ();
  o[X_AXIS] = stem_l->relative_coordinate (0, X_AXIS);

  /*
    Simply set x to middle of notehead
   */

  o[X_AXIS] -= 0.5 * stem_dir * col->extent (X_AXIS).length ();

  if ((stem_dir == dir)
      && !stem_l->extent (Y_AXIS).empty_b ())
    {
      o[Y_AXIS] = stem_l->extent (Y_AXIS)[dir];
    }
  else
    {
      o[Y_AXIS] = col->extent (Y_AXIS)[dir];
    }

  /*
   leave a gap: slur mustn't touch head/stem
   */
  o[Y_AXIS] += dir * paper_l ()->get_var ("slur_y_free");
  o[Y_AXIS] -= calc_interstaff_dist (stem_l, this);
  return o;
}

void
Slur::after_line_breaking ()
{
  set_extremities ();
  set_control_points ();
} 

/*
  urg
  FIXME
 */
void
Slur::set_extremities ()
{
  Link_array<Note_column> encompass_arr =
    Group_interface__extract_elements (this, (Note_column*)0, "note-columns");

  if (!encompass_arr.size ())
    {
      set_elt_property ("transparent", SCM_BOOL_T);
      set_empty (X_AXIS);
      set_empty (Y_AXIS);
      return;
    }

  if (!directional_element (this).get ())
    directional_element (this).set (get_default_dir ());


  /* 
   Slur and tie placement [OSU]

   Slurs:
   * x = centre of head - d * x_gap_f

   TODO:
   * y = length < 5ss : horizontal tangent + d * 0.25 ss
     y = length >= 5ss : y next interline - d * 0.25 ss
   */

  Real staff_space = paper_l ()->get_var ("interline");
  Real half_staff_space = staff_space / 2;

  Real x_gap_f = paper_l ()->get_var ("slur_x_gap");
  Real y_gap_f = paper_l ()->get_var ("slur_y_gap");

  Drul_array<Note_column*> note_column_drul;
  note_column_drul[LEFT] = encompass_arr[0];
  note_column_drul[RIGHT] = encompass_arr.top ();

  bool fix_broken_b = false;

  Direction my_dir = directional_element (this).get ();
  
  Direction d = LEFT;
  do 
    {
      dx_f_drul_[d] = 0;
      dy_f_drul_[d] = 0;
      
      if ((note_column_drul[d] == get_bound (d))
	  && note_column_drul[d]->first_head ()
	  && (note_column_drul[d]->stem_l ()))
	{
	  Stem* stem_l = note_column_drul[d]->stem_l ();
	  /*
	    side directly attached to note head;
	    no beam getting in the way
	  */
	  if ((stem_l->extent (Y_AXIS).empty_b ()
	       || !((stem_l->get_direction () == my_dir) && (my_dir != d)))
	      && !((my_dir == stem_l->get_direction ())
		   && stem_l->beam_l () && (stem_l->beam_count (-d) >= 1)))
	    {
	      dx_f_drul_[d] = get_bound (d)->extent (X_AXIS).length () / 2;
	      dx_f_drul_[d] -= d * x_gap_f;

	      if (stem_l->get_direction () != my_dir)
		{
		  dy_f_drul_[d] = note_column_drul[d]->extent (Y_AXIS)[my_dir];
		}
	      else
		{
		  dy_f_drul_[d] = stem_l->chord_start_f ()
		    + my_dir * half_staff_space;
		}
	      dy_f_drul_[d] += my_dir * y_gap_f;
	    }
	  /*
	    side attached to (visible) stem
	  */
	  else
	    {
	      dx_f_drul_[d] = stem_l->relative_coordinate (0, X_AXIS)
		- get_bound (d)->relative_coordinate (0, X_AXIS);
	      /*
		side attached to beamed stem
	       */
	      if (stem_l->beam_l () && (stem_l->beam_count (-d) >= 1))
		{
		  dy_f_drul_[d] = stem_l->extent (Y_AXIS)[my_dir];
		  dy_f_drul_[d] += my_dir * 2 * y_gap_f;
		}
	      /*
		side attached to notehead, with stem getting in the way
	       */
	      else
		{
		  dx_f_drul_[d] -= d * x_gap_f;
		  
		  dy_f_drul_[d] = stem_l->chord_start_f ()
		    + my_dir * half_staff_space;
		  dy_f_drul_[d] += my_dir * y_gap_f;
		}
	    }
	}
      /*
	loose end
      */
      else
	{
	  dx_f_drul_[d] = get_broken_left_end_align ();
	  	
	  /*
	    broken: should get y from other piece, so that slur
	    continues up/down trend

	    for now: be horizontal..
	  */
	  fix_broken_b = true;
	}
    }
  while (flip (&d) != LEFT);

  int cross_count =  cross_staff_count ();
  bool interstaff_b = (0 < cross_count) && (cross_count < encompass_arr.size ());

  Drul_array<Offset> info_drul;
  Drul_array<Real> interstaff_interval;

  do
    {
      info_drul[d] = encompass_offset (encompass_arr.boundary (d, 0));
      interstaff_interval[d] = - calc_interstaff_dist (encompass_arr.boundary (d,0),
						     this);
    }
  while (flip (&d) != LEFT);
  
  Real interstaff_f = interstaff_interval[RIGHT] - interstaff_interval[LEFT];

  if (fix_broken_b)
    {
      Direction d = (encompass_arr.top () != get_bound (RIGHT)) ?
	RIGHT : LEFT;
      dy_f_drul_[d] = info_drul[d][Y_AXIS];
      if (!interstaff_b)
	{
	  dy_f_drul_[d] -= interstaff_interval[d];
	  if (cross_count)	// interstaff_i  ? 
	    {
	      dy_f_drul_[LEFT] += interstaff_interval[d];
	      dy_f_drul_[RIGHT] += interstaff_interval[d];
	    }
	}
    }
	
  if (!fix_broken_b)
    dy_f_drul_[RIGHT] += interstaff_f;
}


int
Slur::cross_staff_count ()const
{
  Link_array<Note_column> encompass_arr =
    Group_interface__extract_elements (this, (Note_column*)0, "note-columns");

  int k=0;

  for (int i = 0; i < encompass_arr.size (); i++)
    {
      if (calc_interstaff_dist (encompass_arr[i], this))
	k++;
    }
  return k;
}


Array<Offset>
Slur::get_encompass_offset_arr () const
{
  Link_array<Note_column> encompass_arr =
    Group_interface__extract_elements (this, (Note_column*)0, "note-columns");
  
  Array<Offset> offset_arr;
#if 0
  /*
    check non-disturbed slur
    FIXME: x of ends off by a tiny bit!!
  */
  offset_arr.push (Offset (0, dy_f_drul_[LEFT]));
  offset_arr.push (Offset (0, dy_f_drul_[RIGHT]));
  return offset_arr;
#endif
  
  Offset origin (relative_coordinate (0, X_AXIS), 0);

  int first = 1;
  int last = encompass_arr.size () - 2;

  offset_arr.push (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));

  /*
    left is broken edge
  */

  int cross_count  = cross_staff_count ();
  bool cross_b = cross_count && cross_count < encompass_arr.size ();
  if (encompass_arr[0] != get_bound (LEFT))
    {
      first--;
      Real is   = calc_interstaff_dist (encompass_arr[0], this);
      if (cross_b)
	offset_arr[0][Y_AXIS] += is;
    }

  /*
    right is broken edge
  */
  if (encompass_arr.top () != get_bound (RIGHT))
    {
      last++;
    }

  for (int i = first; i <= last; i++)
    {
      Offset o (encompass_offset (encompass_arr[i]));
      offset_arr.push (o - origin);
    }

  offset_arr.push (Offset (spanner_length ()+  dx_f_drul_[RIGHT],
			   dy_f_drul_[RIGHT]));

  return offset_arr;
}


Array<Rod>
Slur::get_rods () const
{
  Array<Rod> a;
  Rod r;
  r.item_l_drul_[LEFT] = get_bound (LEFT);
  r.item_l_drul_[RIGHT] = get_bound (RIGHT);
  
  r.distance_f_ = paper_l ()->get_var ("slur_x_minimum");

  a.push (r);
  return a;
}



/*
  Ugh should have dash-length + dash-period
 */
Molecule
Slur::do_brew_molecule () const
{
  Real thick = paper_l ()->get_var ("slur_thickness");
  Bezier one = get_curve ();

  Molecule a;
  SCM d =  get_elt_property ("dashed");
  if (gh_number_p (d))
    a = lookup_l ()->dashed_slur (one, thick, thick * gh_scm2double (d));
  else
    a = lookup_l ()->slur (one, directional_element (this).get () * thick, thick);

  return a;
}

void
Slur::set_control_points ()
{
  Slur_bezier_bow bb (get_encompass_offset_arr (),
		      directional_element (this).get ());

  Real staff_space = Staff_symbol_referencer_interface (this).staff_space ();
  Real h_inf = paper_l ()->get_var ("slur_height_limit_factor") * staff_space;
  Real r_0 = paper_l ()->get_var ("slur_ratio");

  bb.set_default_bezier (h_inf, r_0);

  if (bb.fit_factor () > 1.0)
    {
      Real length = bb.curve_.control_[3][X_AXIS]; 
      Real default_height = bb.get_default_height (h_inf, r_0, length);
      bb.minimise_enclosed_area (paper_l(), default_height);
      
      Real bff = paper_l ()->get_var ("slur_force_blowfit");
      bb.curve_.control_[1][Y_AXIS] *= bff;
      bb.curve_.control_[2][Y_AXIS] *= bff;
      bb.blow_fit ();

      Real sb = paper_l ()->get_var ("slur_beautiful");
      Real beautiful = length * default_height * sb;
      Real area = bb.enclosed_area_f ();
      
      /*
	Slurs that fit beautifully are not ugly
      */
      if (area > beautiful)
	de_uglyfy (&bb, default_height);
    }

  Bezier b = bb.get_bezier ();


  SCM controls = SCM_EOL;
  for (int i= 4; i--;)
    controls = gh_cons ( ly_offset2scm (b.control_[i]), controls);

  set_elt_property ("control-points", controls);
}
  
  
Bezier
Slur::get_curve () const
{
  Bezier b;
  int i = 0;
  for (SCM s= get_elt_property ("control-points"); s != SCM_EOL; s = gh_cdr (s))
    {
      b.control_[i] = ly_scm2offset (gh_car (s));
      i++;
    }
  
  Array<Offset> enc (get_encompass_offset_arr ());
  Direction dir = directional_element (this).get ();
  
  Real x1 = enc[0][X_AXIS];
  Real x2 = enc.top ()[X_AXIS];
  
  Real off = 0.0;
  for (int i=1; i < enc.size ()-1; i++)
    {
      Real x = enc[i][X_AXIS];
      if (x > x1 && x <x2)
	{
	  Real y = b.get_other_coordinate (X_AXIS, x);
	  off = off >? dir *  (enc[i][Y_AXIS] - y);
	}
    }
  b.translate (Offset (0, dir * off));
  return b;
}

