/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  [TODO]
    * fix broken interstaff slurs
    * begin and end should be treated as a/acknowledge Scripts.
    * broken slur should have uniform trend
    * smart changing of endings
    * smart changing of (Y-?)offsets to avoid ugly beziers
       (along-side-stem)
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
#include "slur-bezier-bow.hh"
#include "main.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"

Slur::Slur (SCM s)
  : Spanner (s)
{
  set_elt_property ("attachment", gh_cons (SCM_BOOL_F, SCM_BOOL_F));
  set_elt_property ("note-columns", SCM_EOL);
  set_elt_property ("control-points", SCM_EOL);
}

void
Slur::add_column (Note_column*n)
{
  if (!gh_pair_p (n->get_elt_property ("note-heads")))
    warning (_ ("Putting slur over rest.  Ignoring."));
  else
    {
      Pointer_group_interface (this, "note-columns").add_element (n);
      add_dependency (n);
    }

  add_bound_item (this, n);
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
    Pointer_group_interface__extract_elements (this, (Note_column*)0, "note-columns");
  
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





Offset
Slur::encompass_offset (Score_element* col,
			Score_element **common) const
{
  Offset o;
  Score_element* stem_l = unsmob_element (col->get_elt_property ("stem"));
  
  Direction dir = Directional_element_interface (this).get ();
  
  if (!stem_l)
    {
      warning (_ ("Slur over rest?"));
      o[X_AXIS] = col->relative_coordinate (common[X_AXIS], X_AXIS);
      o[Y_AXIS] = col->relative_coordinate (common[Y_AXIS], Y_AXIS);
      return o;  
    }
  Direction stem_dir = Directional_element_interface (stem_l).get ();
  o[X_AXIS] = stem_l->relative_coordinate (0, X_AXIS);

  /*
    Simply set x to middle of notehead
   */

  o[X_AXIS] -= 0.5 * stem_dir * col->extent (X_AXIS).length ();

  if ((stem_dir == dir)
      && !stem_l->extent (Y_AXIS).empty_b ())
    {
      o[Y_AXIS] = stem_l->relative_coordinate (common[Y_AXIS], Y_AXIS); // iuhg
    }
  else
    {
      o[Y_AXIS] = col->relative_coordinate (common[Y_AXIS], Y_AXIS);	// ugh
    }

  /*
   leave a gap: slur mustn't touch head/stem
   */
  o[Y_AXIS] += dir * paper_l ()->get_var ("slur_y_free");
  return o;
}

MAKE_SCHEME_CALLBACK(Slur,after_line_breaking);

SCM
Slur::after_line_breaking (SCM smob)
{
  Score_element *me = unsmob_element (smob);
  Slur * sl = dynamic_cast<Slur*>(me);
  sl->set_extremities ();
  sl->set_control_points ();
  return SCM_UNDEFINED;
} 

void
Slur::set_extremities ()
{
  if (!Directional_element_interface (this).get ())
    Directional_element_interface (this).set (get_default_dir ());

  Direction dir = LEFT;
  do 
    {
      if (!gh_symbol_p (index_cell (get_elt_property ("attachment"), dir)))
	{
	  
	  // for (SCM s = get_elt_property ("slur-extremity-rules"); s != SCM_EOL; s = gh_cdr (s))
	  for (SCM s = scm_eval (ly_symbol2scm ("slur-extremity-rules"));
	       s != SCM_EOL; s = gh_cdr (s))
	    {
	      SCM r = gh_call2 (gh_caar (s), this->self_scm_,
				 gh_int2scm ((int)dir));
	      if (r != SCM_BOOL_F)
		{
		  index_set_cell (get_elt_property ("attachment"), dir,
				  gh_cdar (s));
		  break;
		}
	    }
	}
    }
  while (flip (&dir) != LEFT);
}

Offset
Slur::get_attachment (Direction dir,
		      Score_element **common) const
{
  SCM s = get_elt_property ("attachment");
  SCM a = dir == LEFT ? gh_car (s) : gh_cdr (s);
  String str = ly_symbol2string (a);
  Real ss = Staff_symbol_referencer::staff_space ((Score_element*)this);
  Real hs = ss / 2.0;
  Offset o;

  
  if (Note_column* n = dynamic_cast<Note_column*> (get_bound (dir)))
    {
      if (n->stem_l ())
	{
	  Score_element*st = n->stem_l();
	  if (str == "head")
	    {
	      o = Offset (0, Stem::chord_start_f (st ));
	      /*
		Default position is centered in X, on outer side of head Y
	       */
	      o += Offset (0.5 * n->extent (X_AXIS).length (),
			   0.5 * ss * Directional_element_interface (this).get ());
	    }
	  else if (str == "alongside-stem")
	    {
	      o = Offset (0, Stem::chord_start_f (st ));
	      /*
		Default position is on stem X, on outer side of head Y
	       */
	      o += Offset (n->extent (X_AXIS).length ()
			   * (1 + Stem::get_direction (st )),
			   0.5 * ss * Directional_element_interface (this).get ());
	    }
	  else if (str == "stem")
	    {
	      o = Offset (0, Stem::stem_end_position (st ) * hs);
	      /*
		Default position is on stem X, at stem end Y
	       */
	      o += Offset (0.5 *
			   (n->extent (X_AXIS).length ()
			    - st->extent (X_AXIS).length ())
			    * (1 + Stem::get_direction (st )),
			    0);
	    }
	  else if (str == "loose-end")
	    {
	      SCM other_a = dir == LEFT ? gh_cdr (s) : gh_car (s);
	      if (ly_symbol2string (other_a) != "loose-end")
		{
		  o = Offset (0, get_attachment (-dir, common)[Y_AXIS]);
		}
	    }

	  
	  SCM l = scm_assoc
	    (scm_listify (a,
			  gh_int2scm (Stem::get_direction (st ) * dir),
			  gh_int2scm (Directional_element_interface (this).get () * dir),
			  SCM_UNDEFINED),
	     scm_eval (ly_symbol2scm ("slur-extremity-offset-alist")));
	  
	  if (l != SCM_BOOL_F)
	    {
	      o += ly_scm2offset (gh_cdr (l)) * ss * dir;
	    }
	}
    }


  /*
    What if get_bound () is not a note-column?
   */
  if (str != "loose-end"
      && get_bound (dir)->common_refpoint (common[Y_AXIS], Y_AXIS) == common[Y_AXIS])
    {      
      o[Y_AXIS] += get_bound (dir)->relative_coordinate (common[Y_AXIS], Y_AXIS) 
	- relative_coordinate (common[Y_AXIS], Y_AXIS);
    }
  return o;
}

Array<Offset>
Slur::get_encompass_offset_arr () const
{
  SCM eltlist = get_elt_property ("note-columns");
  Score_element *common[] = {common_refpoint (eltlist,X_AXIS),
			     common_refpoint (eltlist,Y_AXIS)};


  common[X_AXIS] = common[X_AXIS]->common_refpoint (get_bound (RIGHT), X_AXIS);
  common[X_AXIS] = common[X_AXIS]->common_refpoint (get_bound (LEFT), X_AXIS);
  
  Link_array<Score_element>  encompass_arr;
  while (gh_pair_p (eltlist))
    {
      encompass_arr.push (unsmob_element (gh_car (eltlist)));      
      eltlist =gh_cdr (eltlist);
    }
  encompass_arr.reverse ();

  
  Array<Offset> offset_arr;

  Offset origin (relative_coordinate (common[X_AXIS], X_AXIS),
		 relative_coordinate (common[Y_AXIS], Y_AXIS)); 

  int first = 1;
  int last = encompass_arr.size () - 2;

  offset_arr.push (get_attachment (LEFT, common));

  /*
    left is broken edge
  */

  if (encompass_arr[0] != get_bound (LEFT))
    {
      first--;

      // ?
      offset_arr[0][Y_AXIS] -=
	encompass_arr[0]->relative_coordinate (common[Y_AXIS], Y_AXIS) 
	- relative_coordinate (common[Y_AXIS], Y_AXIS); 
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
      Offset o (encompass_offset (encompass_arr[i], common));
      offset_arr.push (o - origin);
    }

  offset_arr.push (Offset (spanner_length (), 0) + get_attachment (RIGHT,common));

  if (encompass_arr[0] != get_bound (LEFT))
    {
      offset_arr.top ()[Y_AXIS] -= encompass_arr.top ()->relative_coordinate (common[Y_AXIS], Y_AXIS) 
	- relative_coordinate (common[Y_AXIS], Y_AXIS);
    }

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
MAKE_SCHEME_CALLBACK(Slur,brew_molecule);
SCM
Slur::brew_molecule (SCM smob)
{
  Slur * me = dynamic_cast<Slur*> (unsmob_element (smob));

  
  Real thick = me->paper_l ()->get_var ("slur_thickness");
  Bezier one = me->get_curve ();

  Molecule a;
  SCM d =  me->get_elt_property ("dashed");
  if (gh_number_p (d))
    a = me->lookup_l ()->dashed_slur (one, thick, thick * gh_scm2double (d));
  else
    a = me->lookup_l ()->slur (one, Directional_element_interface (me).get () * thick, thick);

  return a.create_scheme();
}

void
Slur::set_control_points ()
{
  Slur_bezier_bow bb (get_encompass_offset_arr (),
		      Directional_element_interface (this).get ());

  Real staff_space = Staff_symbol_referencer::staff_space (this);
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

  if (!Directional_element_interface (this).get ()
      || ! gh_symbol_p (index_cell (get_elt_property ("attachment"), LEFT)))
    ((Slur*)this)->set_extremities ();
  
  if (!gh_pair_p (get_elt_property ("control-points")))
    ((Slur*)this)->set_control_points ();
  
  
  for (SCM s= get_elt_property ("control-points"); s != SCM_EOL; s = gh_cdr (s))
    {
      b.control_[i] = ly_scm2offset (gh_car (s));
      i++;
    }
  
  Array<Offset> enc (get_encompass_offset_arr ());
  Direction dir = Directional_element_interface (this).get ();
  
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

