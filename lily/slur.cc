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
#include "spanner.hh"


void
Slur::set_interface (Score_element*me)
{
  me->set_elt_property ("attachment", gh_cons (SCM_BOOL_F, SCM_BOOL_F));
  me->set_interface (ly_symbol2scm ("slur-interface"));
}

void
Slur::add_column (Score_element*me, Score_element*n)
{
  if (!gh_pair_p (n->get_elt_property ("note-heads")))
    warning (_ ("Putting slur over rest.  Ignoring."));
  else
    {
      Pointer_group_interface (me, "note-columns").add_element (n);
      me->add_dependency (n);
    }

  add_bound_item (dynamic_cast<Spanner*> (me), dynamic_cast<Item*>(n));
}

void
Slur::de_uglyfy (Score_element*me, Slur_bezier_bow* bb, Real default_height)
{
  Real length = bb->curve_.control_[3][X_AXIS] ; 
  Real ff = bb->fit_factor ();
  for (int i = 1; i < 3; i++)
    {
      Real ind = abs (bb->curve_.control_[(i-1)*3][X_AXIS]
		      - bb->curve_.control_[i][X_AXIS]) / length;
      Real h = bb->curve_.control_[i][Y_AXIS] * ff / length;

      Real f = default_height / length;
      Real c1 = me->paper_l ()->get_var ("bezier_control1");
      Real c2 = me->paper_l ()->get_var ("bezier_control2");
      Real c3 = me->paper_l ()->get_var ("bezier_control3");
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
Slur::get_default_dir (Score_element*me) 
{
  Link_array<Score_element> encompass_arr =
    Pointer_group_interface__extract_elements (me, (Score_element*)0, "note-columns");
  
  Direction d = DOWN;
  for (int i=0; i < encompass_arr.size (); i ++) 
    {
      if (Note_column::dir (encompass_arr[i]) < 0) 
	{
	  d = UP;
	  break;
	}
    }
  return d;
}





Offset
Slur::encompass_offset (Score_element*me,
			Score_element* col,
			Score_element **common) 
{
  Offset o;
  Score_element* stem_l = unsmob_element (col->get_elt_property ("stem"));
  
  Direction dir = Directional_element_interface (me).get ();
  
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
  o[Y_AXIS] += dir * me->paper_l ()->get_var ("slur_y_free");
  return o;
}

MAKE_SCHEME_CALLBACK(Slur,after_line_breaking);

SCM
Slur::after_line_breaking (SCM smob)
{
  Score_element *me = unsmob_element (smob);
  set_extremities (me);
  set_control_points (me);
  return SCM_UNSPECIFIED;
} 

void
Slur::set_extremities (Score_element*me)
{
  if (!Directional_element_interface (me).get ())
    Directional_element_interface (me).set (get_default_dir (me));

  Direction dir = LEFT;
  do 
    {
      if (!gh_symbol_p (index_cell (me->get_elt_property ("attachment"), dir)))
	{
	  
	  // for (SCM s = get_elt_property ("slur-extremity-rules"); s != SCM_EOL; s = gh_cdr (s))
	  for (SCM s = scm_eval (ly_symbol2scm ("slur-extremity-rules"));
	       s != SCM_EOL; s = gh_cdr (s))
	    {
	      SCM r = gh_call2 (gh_caar (s), me->self_scm_,
				 gh_int2scm ((int)dir));
	      if (r != SCM_BOOL_F)
		{
		  index_set_cell (me->get_elt_property ("attachment"), dir,
				  gh_cdar (s));
		  break;
		}
	    }
	}
    }
  while (flip (&dir) != LEFT);
}

Offset
Slur::get_attachment (Score_element*me,Direction dir,
		      Score_element **common) 
{
  Spanner*sp = dynamic_cast<Spanner*>(me);
  SCM s = me->get_elt_property ("attachment");
  SCM a = dir == LEFT ? gh_car (s) : gh_cdr (s);
  String str = ly_symbol2string (a);
  Real ss = Staff_symbol_referencer::staff_space ((Score_element*)me);
  Real hs = ss / 2.0;
  Offset o;


  if (Note_column::has_interface (sp->get_bound (dir)))
    {
      Score_element * n =sp->get_bound (dir);
      if (Score_element*st = Note_column::stem_l (n))
	{

	  if (str == "head")
	    {
	      o = Offset (0, Stem::chord_start_f (st ));
	      /*
		Default position is centered in X, on outer side of head Y
	       */
	      o += Offset (0.5 * n->extent (X_AXIS).length (),
			   0.5 * ss * Directional_element_interface (me).get ());
	    }
	  else if (str == "alongside-stem")
	    {
	      o = Offset (0, Stem::chord_start_f (st ));
	      /*
		Default position is on stem X, on outer side of head Y
	       */
	      o += Offset (n->extent (X_AXIS).length ()
			   * (1 + Stem::get_direction (st )),
			   0.5 * ss * Directional_element_interface (me).get ());
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
		  o = Offset (0, get_attachment (me, -dir, common)[Y_AXIS]);
		}
	    }

	  
	  SCM l = scm_assoc
	    (scm_listify (a,
			  gh_int2scm (Stem::get_direction (st ) * dir),
			  gh_int2scm (Directional_element_interface (me).get () * dir),
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
      && sp->get_bound (dir)->common_refpoint (common[Y_AXIS], Y_AXIS) == common[Y_AXIS])
    {      
      o[Y_AXIS] += sp->get_bound (dir)->relative_coordinate (common[Y_AXIS], Y_AXIS) 
	- me->relative_coordinate (common[Y_AXIS], Y_AXIS);
    }
  return o;
}

Array<Offset>
Slur::get_encompass_offset_arr (Score_element*me) 
{
    Spanner*sp = dynamic_cast<Spanner*>(me);
  SCM eltlist = me->get_elt_property ("note-columns");
  Score_element *common[] = {me->common_refpoint (eltlist,X_AXIS),
			     me->common_refpoint (eltlist,Y_AXIS)};


  common[X_AXIS] = common[X_AXIS]->common_refpoint (sp->get_bound (RIGHT), X_AXIS);
  common[X_AXIS] = common[X_AXIS]->common_refpoint (sp->get_bound (LEFT), X_AXIS);
  
  Link_array<Score_element>  encompass_arr;
  while (gh_pair_p (eltlist))
    {
      encompass_arr.push (unsmob_element (gh_car (eltlist)));      
      eltlist =gh_cdr (eltlist);
    }
  encompass_arr.reverse ();

  
  Array<Offset> offset_arr;

  Offset origin (me->relative_coordinate (common[X_AXIS], X_AXIS),
		 me->relative_coordinate (common[Y_AXIS], Y_AXIS)); 

  int first = 1;
  int last = encompass_arr.size () - 2;

  offset_arr.push (get_attachment (me, LEFT, common));

  /*
    left is broken edge
  */

  if (encompass_arr[0] != sp->get_bound (LEFT))
    {
      first--;

      // ?
      offset_arr[0][Y_AXIS] -=
	encompass_arr[0]->relative_coordinate (common[Y_AXIS], Y_AXIS) 
	- me->relative_coordinate (common[Y_AXIS], Y_AXIS); 
    }

  /*
    right is broken edge
  */
  if (encompass_arr.top () != sp->get_bound (RIGHT))
    {
      last++;
    }

  for (int i = first; i <= last; i++)
    {
      Offset o (encompass_offset (me, encompass_arr[i], common));
      offset_arr.push (o - origin);
    }

  offset_arr.push (Offset (sp->spanner_length (), 0) + get_attachment (me, RIGHT,common));

  if (encompass_arr[0] != sp->get_bound (LEFT))
    {
      offset_arr.top ()[Y_AXIS] -= encompass_arr.top ()->relative_coordinate (common[Y_AXIS], Y_AXIS) 
	- me->relative_coordinate (common[Y_AXIS], Y_AXIS);
    }

  return offset_arr;
}


MAKE_SCHEME_CALLBACK(Slur,set_spacing_rods);
SCM
Slur::set_spacing_rods (SCM smob)
{
  Score_element*me = unsmob_element (smob);

  Rod r;
  Spanner*sp = dynamic_cast<Spanner*>(me);
  r.item_l_drul_[LEFT] = sp->get_bound (LEFT);
  r.item_l_drul_[RIGHT] = sp->get_bound (RIGHT);
  r.distance_f_ =
    gh_scm2double (me->get_elt_property ("minimum-length"))
    * me->paper_l ()->get_var ("staffspace");

  r.add_to_cols ();
  return SCM_UNSPECIFIED;
}


/*
  Ugh should have dash-length + dash-period
 */
MAKE_SCHEME_CALLBACK(Slur,brew_molecule);
SCM
Slur::brew_molecule (SCM smob)
{
  Score_element * me = unsmob_element (smob);
  Real thick = me->paper_l ()->get_var ("stafflinethickness") *
    gh_scm2double (me->get_elt_property ("thickness"));
  Bezier one = get_curve (me);

  Molecule a;
  SCM d =  me->get_elt_property ("dashed");
  if (gh_number_p (d))
    a = me->lookup_l ()->dashed_slur (one, thick, thick * gh_scm2double (d));
  else
    a = me->lookup_l ()->slur (one, Directional_element_interface (me).get () * thick, thick);

  return a.create_scheme();
}

void
Slur::set_control_points (Score_element*me)
{
  Real staff_space = Staff_symbol_referencer::staff_space ((Score_element*)me);  
  Real h_inf = me->paper_l ()->get_var ("slur_height_limit_factor") *
    staff_space;
  Real r_0 = me->paper_l ()->get_var ("slur_ratio");
  
  Slur_bezier_bow bb (get_encompass_offset_arr (me),
		      Directional_element_interface (me).get (),
		      h_inf, r_0);

  if (bb.fit_factor () > 1.0)
    {
      Real length = bb.curve_.control_[3][X_AXIS]; 
      Real default_height = slur_height (length, h_inf, r_0);
      bb.minimise_enclosed_area (me->paper_l());
      
      Real bff = me->paper_l ()->get_var ("slur_force_blowfit");
      bb.curve_.control_[1][Y_AXIS] *= bff;
      bb.curve_.control_[2][Y_AXIS] *= bff;
      bb.blow_fit ();

      Real sb = me->paper_l ()->get_var ("slur_beautiful");
      Real beautiful = length * default_height * sb;
      Real area = bb.enclosed_area_f ();
      
      /*
	Slurs that fit beautifully are not ugly
      */
      if (area > beautiful)
	de_uglyfy (me, &bb, default_height);
    }

  Bezier b = bb.get_bezier ();


  SCM controls = SCM_EOL;
  for (int i= 4; i--;)
    controls = gh_cons ( ly_offset2scm (b.control_[i]), controls);

  me->set_elt_property ("control-points", controls);
}
  
  
Bezier
Slur::get_curve (Score_element*me) 
{
  Bezier b;
  int i = 0;

  if (!Directional_element_interface (me).get ()
      || ! gh_symbol_p (index_cell (me->get_elt_property ("attachment"), LEFT)))
    set_extremities (me);
  
  if (!gh_pair_p (me->get_elt_property ("control-points")))
    set_control_points (me);
  
  
  for (SCM s= me->get_elt_property ("control-points"); s != SCM_EOL; s = gh_cdr (s))
    {
      b.control_[i] = ly_scm2offset (gh_car (s));
      i++;
    }
  
  Array<Offset> enc (get_encompass_offset_arr (me));
  Direction dir = Directional_element_interface (me).get ();
  
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


bool
Slur::has_interface (Score_element*me)
{
  return me->has_interface (ly_symbol2scm ("slur-interface"));
}


