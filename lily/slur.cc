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
      Pointer_group_interface::add_element (me, "note-columns",n);
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
      SCM up = me->get_elt_property ("de-uglify-parameters");
      
      Real c1 = gh_scm2double (gh_car (up));
      Real c2 = gh_scm2double (gh_cadr (up));
      Real c3 = gh_scm2double (gh_caddr (up)); 
      
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


MAKE_SCHEME_CALLBACK (Slur, after_line_breaking,1);
SCM
Slur::after_line_breaking (SCM smob)
{
  Score_element *me = unsmob_element (smob);
  if (!scm_ilength (me->get_elt_property ("note-columns")))
    {
      me->suicide ();
      return SCM_UNSPECIFIED;
    }
  set_extremities (me);
  set_control_points (me);
  return SCM_UNSPECIFIED;
} 

void
Slur::set_extremities (Score_element*me)
{
  if (!Directional_element_interface::get (me))
    Directional_element_interface ::set (me,get_default_dir (me));

  Direction dir = LEFT;
  do 
    {
      if (!gh_symbol_p (index_cell (me->get_elt_property ("attachment"), dir)))
	{
	  for (SCM s = me->get_elt_property ("extremity-rules");
	       s != SCM_EOL; s = gh_cdr (s))
	    {
	      SCM r = gh_call2 (gh_caar (s), me->self_scm (),
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

Real
Slur::get_first_notecolumn_y (Score_element *me, Direction dir)
{
  Score_element *col = dir == LEFT
    ? unsmob_element (gh_car (scm_reverse (me->get_elt_property
					   ("note-columns"))))
    : unsmob_element
    (gh_car (me->get_elt_property ("note-columns")));
  
  Score_element *common[] =
  {
    0,
    me->common_refpoint (col, Y_AXIS)
  };
  Real y;
  if (col == ((Spanner*)me)->get_bound (dir))
    {
      y = get_attachment (me, dir, common)[Y_AXIS];
    }
  else
    {
      y = encompass_offset (me, col, common)[Y_AXIS]
	- me->relative_coordinate (common[Y_AXIS], Y_AXIS); 
    }
  return y;
}

Offset
Slur::broken_trend_offset (Score_element *me, Direction dir)
{
  /*
    A broken slur should maintain the same vertical trend
    the unbroken slur would have had.
  */
  Offset o;
  if (Spanner *mother =  dynamic_cast<Spanner*> (me->original_l_))
    {
      for (int i = dir == LEFT ? 0 : mother->broken_into_l_arr_.size () - 1;
	   dir == LEFT ? i < mother->broken_into_l_arr_.size () : i > 0;
	   dir == LEFT ? i++ : i--)
	{
	  if (mother->broken_into_l_arr_[i - dir] == me)
	    {
	      Score_element *neighbour = mother->broken_into_l_arr_[i];
	      if (dir == RIGHT)
		neighbour->set_elt_property ("direction",
					     me->get_elt_property ("direction"));
	      Real neighbour_y = get_first_notecolumn_y (neighbour, dir);
	      Real y = get_first_notecolumn_y (me, -dir);
	      o = Offset (0, (y + neighbour_y) / 2);
	      break;
	    }
	}
    }
  return o;
}

Offset
Slur::get_attachment (Score_element*me,Direction dir,
		      Score_element **common) 
{
  SCM s = me->get_elt_property ("attachment");
  if (!gh_symbol_p (index_cell (s, dir)))
    {
      set_extremities (me);
      s = me->get_elt_property ("attachment");
    }
  SCM a = dir == LEFT ? gh_car (s) : gh_cdr (s);
  Spanner*sp = dynamic_cast<Spanner*>(me);
  String str = ly_symbol2string (a);
  Real ss = Staff_symbol_referencer::staff_space ((Score_element*)me);
  Real hs = ss / 2.0;
  Offset o;
  
  Score_element *stem = 0;
  if (Note_column::has_interface (sp->get_bound (dir)))
    {
      Score_element * n =sp->get_bound (dir);
      if ((stem = Note_column::stem_l (n)))
	{

	  if (str == "head")
	    {
	      o = Offset (0, Stem::head_positions (stem)
			  [Directional_element_interface::get (me)] * hs);
	      /*
		Default position is centered in X, on outer side of head Y
	       */
	      o += Offset (0.5 * n->extent (n,X_AXIS).length (),
			   0.5 * ss * Directional_element_interface::get (me));
	    }
	  else if (str == "alongside-stem")
	    {
	      o = Offset (0, Stem::chord_start_f (stem));
	      /*
		Default position is on stem X, on outer side of head Y
	       */
	      o += Offset (n->extent (n,X_AXIS).length ()
			   * (1 + Stem::get_direction (stem)),
			   0.5 * ss * Directional_element_interface::get (me));
	    }
	  else if (str == "stem")
	    {
	      o = Offset (0, Stem::stem_end_position (stem) * hs);
	      /*
		Default position is on stem X, at stem end Y
	       */
	      o += Offset (0.5 *
			   (n->extent (n,X_AXIS).length ()
			    - stem->extent (stem,X_AXIS).length ())
			    * (1 + Stem::get_direction (stem)),
			    0);
	    }
	}
    }
  else if (str == "loose-end")
    {
      SCM other_a = dir == LEFT ? gh_cdr (s) : gh_car (s);
      if (ly_symbol2string (other_a) != "loose-end")
	{
#if 0
	  /*
	    The braindead way: horizontal
	  */
	  o = Offset (0, get_attachment (me, -dir, common)[Y_AXIS]);
#else
	  o = broken_trend_offset (me, dir);
#endif

	  
	}
	
    }

  SCM alist = me->get_elt_property ("extremity-offset-alist");
int stemdir = stem ? Stem::get_direction (stem) : 1;
  int slurdir = gh_scm2int (me->get_elt_property ("direction"));
  SCM l = scm_assoc
      (scm_listify (a,
                gh_int2scm (stemdir * dir),
                gh_int2scm (slurdir * dir),
                  SCM_UNDEFINED), alist);

  if (l != SCM_BOOL_F)
    {
      o += ly_scm2offset (gh_cdr (l)) * ss * dir;
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

Offset
Slur::encompass_offset (Score_element*me,
			Score_element* col,
			Score_element **common) 
{
  Offset o;
  Score_element* stem_l = unsmob_element (col->get_elt_property ("stem"));
  
  Direction dir = Directional_element_interface::get (me);
  
  if (!stem_l)
    {
      warning (_ ("Slur over rest?"));
      o[X_AXIS] = col->relative_coordinate (common[X_AXIS], X_AXIS);
      o[Y_AXIS] = col->relative_coordinate (common[Y_AXIS], Y_AXIS);
      return o;  
    }
  Direction stem_dir = Directional_element_interface::get (stem_l);
  o[X_AXIS] = stem_l->relative_coordinate (0, X_AXIS);

  /*
    Simply set x to middle of notehead
   */

  o[X_AXIS] -= 0.5 * stem_dir * col->extent (col,X_AXIS).length ();

  if ((stem_dir == dir)
      && !stem_l->extent (stem_l, Y_AXIS).empty_b ())
    {
      o[Y_AXIS] = stem_l->extent (common[Y_AXIS], Y_AXIS)[dir];
    }
  else
    {
      o[Y_AXIS] = col->extent (common[Y_AXIS], Y_AXIS)[dir];
    }

  /*
   leave a gap: slur mustn't touch head/stem
   */
  o[Y_AXIS] += dir * gh_scm2double (me->get_elt_property ("y-free")) *
    me->paper_l ()->get_var ("staffspace");
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


MAKE_SCHEME_CALLBACK(Slur,set_spacing_rods,1);
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
MAKE_SCHEME_CALLBACK (Slur, brew_molecule,1);
SCM
Slur::brew_molecule (SCM smob)
{
  Score_element * me = unsmob_element (smob);
  if (!scm_ilength (me->get_elt_property ("note-columns")))
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real thick = me->paper_l ()->get_var ("stafflinethickness") *
    gh_scm2double (me->get_elt_property ("thickness"));
  Bezier one = get_curve (me);

  // get_curve may suicide
  if (!scm_ilength (me->get_elt_property ("note-columns")))
    return SCM_EOL;

  Molecule a;
  SCM d =  me->get_elt_property ("dashed");
  if (gh_number_p (d))
    a = me->lookup_l ()->dashed_slur (one, thick, thick * gh_scm2double (d));
  else
    a = me->lookup_l ()->slur (one, Directional_element_interface::get (me) * thick, thick);

  return a.create_scheme();
}

void
Slur::set_control_points (Score_element*me)
{
  Real staff_space = Staff_symbol_referencer::staff_space ((Score_element*)me);

  SCM details = me->get_elt_property ("details");
  SCM h_inf_scm = scm_assq (ly_symbol2scm ("height-limit"), details);
  SCM r_0_scm = scm_assq (ly_symbol2scm ("ratio"), details);

  Real r_0 = gh_scm2double (gh_cdr (r_0_scm));
  Real h_inf = staff_space * gh_scm2double (gh_cdr (h_inf_scm));
  
  Slur_bezier_bow bb (get_encompass_offset_arr (me),
		      Directional_element_interface::get (me),
		      h_inf, r_0);

  if (bb.fit_factor () > 1.0)
    {
      Real length = bb.curve_.control_[3][X_AXIS]; 
      Real default_height = slur_height (length, h_inf, r_0);

      SCM ssb = scm_assq (ly_symbol2scm ("beautiful"), details);
      Real sb =gh_scm2double (gh_cdr (ssb));

      bb.minimise_enclosed_area (me->paper_l(), sb);
      SCM sbf = scm_assq (ly_symbol2scm ("force-blowfit"), details);
      Real bff = 1.0;
      if (gh_pair_p (sbf) && gh_number_p (gh_cdr (sbf)))
	  bff = gh_scm2double (gh_cdr (sbf));

      bb.curve_.control_[1][Y_AXIS] *= bff;
      bb.curve_.control_[2][Y_AXIS] *= bff;
      bb.blow_fit ();

      
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
    {
      controls = gh_cons ( ly_offset2scm (b.control_[i]), controls);
      /*
	BRRR WHURG.
	All these null control-points, where do they all come from?
      */
      if (i && b.control_[i][X_AXIS] == 0)
	{
	  me->suicide ();
	  return;
	}
    }

  me->set_elt_property ("control-points", controls);
}
  
Bezier
Slur::get_curve (Score_element*me) 
{
  Bezier b;
  int i = 0;

  if (!Directional_element_interface::get (me)
      || ! gh_symbol_p (index_cell (me->get_elt_property ("attachment"), LEFT))
      || ! gh_symbol_p (index_cell (me->get_elt_property ("attachment"), RIGHT)))
    set_extremities (me);
  
  if (!gh_pair_p (me->get_elt_property ("control-points")))
    set_control_points (me);

  // set_control_points may suicide
  if (!scm_ilength (me->get_elt_property ("note-columns")))
    return b;

  for (SCM s= me->get_elt_property ("control-points"); s != SCM_EOL; s = gh_cdr (s))
    {
      b.control_[i] = ly_scm2offset (gh_car (s));
      i++;
    }

  Array<Offset> enc (get_encompass_offset_arr (me));
  Direction dir = Directional_element_interface::get (me);
  
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


