/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
#include "bezier.hh"
#include "bezier-bow.hh"
#include "main.hh"
#include "cross-staff.hh"
#include "group-interface.hh"

Slur::Slur ()
{
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
  set_elt_property ("note-columns", SCM_EOL);
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
  set_bounds (LEFT, encompass_arr[0]);    
  if (encompass_arr.size () > 1)
    set_bounds (RIGHT, encompass_arr.top ());
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
      o[X_AXIS] = col->hpos_f ();
      o[Y_AXIS] = col->extent (Y_AXIS)[dir];
      return o;  
    }
  Direction stem_dir = directional_element (stem_l).get ();
  o[X_AXIS] = stem_l->hpos_f ();

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

/*
  ARGRARGRARGRARGAR!

  Fixme
 */
void
Slur::do_post_processing ()
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
      
      if ((note_column_drul[d] == spanned_drul_[d])
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
	      dx_f_drul_[d] = spanned_drul_[d]->extent (X_AXIS).length () / 2;
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
	      dx_f_drul_[d] = stem_l->hpos_f ()
		- spanned_drul_[d]->relative_coordinate (0, X_AXIS);
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
      Direction d = (encompass_arr.top () != spanned_drul_[RIGHT]) ?
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



  return; 

  /*
    Now we've got a fine slur
    Catch and correct some ugly cases
   */
  String infix = interstaff_b ? "interstaff_" : "";
  Real height_damp_f = paper_l ()->get_var ("slur_"+infix +"height_damping");
  Real slope_damp_f = paper_l ()->get_var ("slur_"+infix +"slope_damping");
  Real snap_f = paper_l ()->get_var ("slur_"+infix +"snap_to_stem");
  Real snap_max_dy_f = paper_l ()->get_var ("slur_"+infix +"snap_max_slope_change");

  Real dx_f = spanner_length ()+ dx_f_drul_[RIGHT] - dx_f_drul_[LEFT];
  Real dy_f = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];
  if (!fix_broken_b)
    dy_f -= interstaff_f;

  /*
    Avoid too steep slurs.
   */
  Real slope_ratio_f = abs (dy_f / dx_f);
  if (slope_ratio_f > slope_damp_f)
    {
      Direction d = (Direction)(- my_dir * (sign (dy_f)));
      if (!d)
	d = LEFT;
      Real damp_f = (slope_ratio_f - slope_damp_f) * dx_f;
      /*
	must never change sign of dy
       */
      damp_f = damp_f <? abs (dy_f);
      dy_f_drul_[d] += my_dir * damp_f;
    }

  /*
   Avoid too high slurs 

   Wierd slurs may look a lot better after they have been
   adjusted a bit.
   So, we'll do this in 3 steps
   */
  for (int i = 0; i < 3; i++)
    {
      Bezier c (get_curve ());
      
      Offset size (c.extent (X_AXIS).length (),
		   c.extent (Y_AXIS).length ());

      dy_f = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];
      if (!fix_broken_b)
	dy_f -= interstaff_f;

      Real height_ratio_f = abs (size[Y_AXIS] / size[X_AXIS]);
      if (height_ratio_f > height_damp_f)
	{
	  Direction d = (Direction)(- my_dir * (sign (dy_f)));
	  if (!d)
	    d = LEFT;
	  /* take third step */
	  Real damp_f = (height_ratio_f - height_damp_f) * size[X_AXIS] / 3;
	  /*
	    if y positions at about the same height, correct both ends
	  */
	  if (abs (dy_f / dx_f ) < slope_damp_f)
	    {
	      dy_f_drul_[-d] += my_dir * damp_f;
	      dy_f_drul_[d] += my_dir * damp_f;
	    }
	  /*
	    don't change slope too much, would have been catched by slope damping
	  */
	  else
	    {
	      damp_f = damp_f <? abs (dy_f/2);
	      dy_f_drul_[d] += my_dir * damp_f;
	    }
	}
    }

  /*
    If, after correcting, we're close to stem-end...
  */
  Drul_array<Real> snapy_f_drul;
  snapy_f_drul[LEFT] = snapy_f_drul[RIGHT] = 0;
  Drul_array<Real> snapx_f_drul;
  snapx_f_drul[LEFT] = snapx_f_drul[RIGHT] = 0;
  Drul_array<bool> snapped_b_drul;
  snapped_b_drul[LEFT] = snapped_b_drul[RIGHT] = false;
  do
    {
      Note_column * nc = note_column_drul[d];
      if (nc == spanned_drul_[d]
	  && nc->stem_l ()
	  && nc->stem_l ()->get_direction () == my_dir
	  && abs (nc->stem_l ()->extent (Y_AXIS)[my_dir]
		  - dy_f_drul_[d] + (d == LEFT ? 0 : interstaff_f))
	      <= snap_f)
	{
	  /*
	    prepare to attach to stem-end
	  */
	  snapx_f_drul[d] = nc->stem_l ()->hpos_f ()
	    - spanned_drul_[d]->relative_coordinate (0, X_AXIS);

	  snapy_f_drul[d] = nc->stem_l ()->extent (Y_AXIS)[my_dir]
	    + interstaff_interval[d]
	    + my_dir * 2 * y_gap_f;
	  
	  snapped_b_drul[d] = true;
	}
    }
  while (flip (&d) != LEFT);

  /*
    only use snapped positions if sign (dy) will not change
    and dy doesn't change too much
    */
  if (!fix_broken_b)
    dy_f += interstaff_f;


  /*
    (sigh)

    More refactoring could be done.
   */
  Real maxsnap = abs (dy_f * snap_max_dy_f);
  if (snapped_b_drul[LEFT] && snapped_b_drul[RIGHT]
      && ((sign (snapy_f_drul[RIGHT] - snapy_f_drul[LEFT]) == sign (dy_f)))
      && (!dy_f || (abs (snapy_f_drul[RIGHT] - snapy_f_drul[LEFT] - dy_f)
		    < maxsnap)))
    {
      dy_f_drul_ = snapy_f_drul;
      dx_f_drul_ = snapx_f_drul;
    }
  else
    do
      {
	Direction od = (Direction)-d;
	if (snapped_b_drul[d]
	    && d * sign (snapy_f_drul[d] - dy_f_drul_[od]) == sign (dy_f)
	    && (!dy_f || (abs (snapy_f_drul[d] - dy_f_drul_[od]  - d * dy_f)
			  < maxsnap)))
	  {
	    dy_f_drul_[d] = snapy_f_drul[d];
	    dx_f_drul_[d] = snapx_f_drul[d];
	  }
      }
    while (flip (&d) != LEFT);
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
  if (encompass_arr[0] != spanned_drul_[LEFT])
    {
      first--;
      Real is   = calc_interstaff_dist (encompass_arr[0], this);
      if (cross_b)
	offset_arr[0][Y_AXIS] += is;
    }

  /*
    right is broken edge
  */
  if (encompass_arr.top () != spanned_drul_[RIGHT])
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
  r.item_l_drul_ = spanned_drul_;
  r.distance_f_ = paper_l ()->get_var ("slur_x_minimum");

  a.push (r);
  return a;
}



#if 0
SCM
ugly_scm (Bezier  b) 
{
  b.translate (-b.control_[0]);
  Real alpha = b.control_[3].arg ();

  b.rotate ( -alpha);
  if (b.control_[1][Y_AXIS] < 0)
    {
      b.control_[1][Y_AXIS] *= -1;
      b.control_[2][Y_AXIS] *= -1;      
    }

  Real len = b.control_[3][X_AXIS];
  Real indent = 10 *b.control_[1][X_AXIS] / len ;
  Real ht = 10 *b.control_[1][Y_AXIS] / len ;
  
  SCM res = scm_eval (scm_listify (ly_symbol2scm ("slur-ugly"), gh_double2scm (indent), gh_double2scm (ht), SCM_UNDEFINED ));

  return res;
}
#endif


/*
  Ugh should have dash-length + dash-period
 */
Molecule*
Slur::do_brew_molecule_p () const
{
  Real thick = paper_l ()->get_var ("slur_thickness");
  Bezier one = get_curve ();

  Molecule a;
  SCM d =  get_elt_property ("dashed");
  if (gh_number_p (d))
    a = lookup_l ()->dashed_slur (one, thick, thick * gh_scm2double (d));
  else
    a = lookup_l ()->slur (one, directional_element (this).get () * thick, thick);

#if 0 
  SCM u = ugly_scm (one);
  if (gh_pair_p (u))
    {
      Molecule mark = lookup_l ()-> text ( "roman",
			   to_str (gh_scm2double (gh_car (u)), "%0.2f") + "," +
			   to_str(gh_scm2double (gh_cdr (u)), "%0.2f"),
			   paper_l ());

      mark.translate_axis (20 , Y_AXIS);
      a.add_molecule (mark);
    }
#endif
  return new Molecule (a); 
}



Bezier
Slur::get_curve () const
{
  Array<Offset> enc (get_encompass_offset_arr ());
  Direction dir =  directional_element (this).get ();
  Bezier_bow b (enc,dir);

  b.ratio_ = paper_l ()->get_var ("slur_ratio");
  b.height_limit_ = paper_l ()->get_var ("slur_height_limit");
  b.rc_factor_ = paper_l ()->get_var ("slur_rc_factor");

  b.calculate ();

  Bezier  curve =  b.get_curve ();

  Real x1 = enc[0][X_AXIS];
  Real x2 = enc.top ()[X_AXIS];
  
  Real off = 0.0;
  for (int i=1; i < enc.size ()-1; i++)
    {
      Real x = enc[i][X_AXIS];
      if (x > x1 && x <x2)
	{
	  Real y = curve.get_other_coordinate (X_AXIS, x);
	  off = off >? dir *  (enc[i][Y_AXIS] - y);
	}
    }
  curve.translate (Offset (0, dir * off));
  return curve;
}

