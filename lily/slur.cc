/*
  slur.cc -- implement Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  [TODO]
  
    * should avoid stafflines with horizontal part.

    * begin and end should be treated as a/acknowledge Scripts.

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
#include "stencil.hh"
#include "warn.hh"
#include "slur-bezier-bow.hh"
#include "main.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "spanner.hh"


void
Slur::set_interface (Grob*me)
{
  /* Copy to mutable list. */
  me->set_property ("attachment",
			 ly_deep_copy (me->get_property ("attachment")));
}

void
Slur::add_column (Grob*me, Grob*n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-columns"), n);
  me->add_dependency (n);

  add_bound_item (dynamic_cast<Spanner*> (me), dynamic_cast<Item*> (n));
}

void
Slur::de_uglyfy (Grob*me, Slur_bezier_bow* bb, Real default_height)
{
  Real length = bb->curve_.control_[3][X_AXIS] ; 
  Real ff = bb->fit_factor ();
  for (int i = 1; i < 3; i++)
    {
      Real ind = abs (bb->curve_.control_[ (i-1)*3][X_AXIS]
		      - bb->curve_.control_[i][X_AXIS]) / length;
      Real h = bb->curve_.control_[i][Y_AXIS] * ff / length;

      Real f = default_height / length;
      SCM up = me->get_property ("de-uglify-parameters");
      
      Real c1 = ly_scm2double (ly_car (up));
      Real c2 = ly_scm2double (ly_cadr (up));
      Real c3 = ly_scm2double (ly_caddr (up)); 
      
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
Slur::get_default_dir (Grob*me) 
{
  Link_array<Grob> encompasses =
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "note-columns");
  
  Direction d = DOWN;
  for (int i=0; i < encompasses.size (); i ++) 
    {
      if (Note_column::dir (encompasses[i]) < 0) 
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
  Grob *me = unsmob_grob (smob);
  if (!scm_ilength (me->get_property ("note-columns")))
    {
      me->suicide ();
      return SCM_UNSPECIFIED;
    }
  set_extremities (me);
  if (!ly_c_pair_p (me->get_property ("control-points")))
    set_control_points (me);
  return SCM_UNSPECIFIED;
} 


void
Slur::check_slope (Grob *me)
{
  /*
    Avoid too steep slurs.
   */
  SCM s = me->get_property ("slope-limit");
  if (is_number (s))
    {
      Array<Offset> encompass = get_encompass_offsets (me);
      Drul_array<Offset> attachment;
      attachment[LEFT] = encompass[0];
      attachment[RIGHT] = encompass.top ();

      Real dx = attachment[RIGHT][X_AXIS] - attachment[LEFT][X_AXIS];
      Real dy = attachment[RIGHT][Y_AXIS] - attachment[LEFT][Y_AXIS];
      if (!dx)
	return;
      
      Real slope = slope = abs (dy / dx);

      Real limit = ly_scm2double (s);

      if (slope > limit)
	{
	  Real staff_space = Staff_symbol_referencer::staff_space ((Grob*)me);
	  Direction dir = (Direction)ly_scm2int (me->get_property ("direction"));
	  Direction d = (Direction) (- dir * (sign (dy)));
	  SCM a = me->get_property ("attachment-offset");
	  Drul_array<Offset> o;
	  o[LEFT] = ly_scm2offset (index_get_cell (a, LEFT));
	  o[RIGHT] = ly_scm2offset (index_get_cell (a, RIGHT));
	  o[d][Y_AXIS] -= (limit - slope) * dx * dir / staff_space;

	  o[d][Y_AXIS] *= get_grob_direction (me);

	  me->set_property ("attachment-offset",
				scm_cons (ly_offset2scm (o[LEFT]),
					 ly_offset2scm (o[RIGHT])));
	}
    }

}

/*
  Set 'attachment grob property, and return it.
*/
SCM
Slur::set_extremities (Grob *me)
{
  if (!get_grob_direction (me))
    set_grob_direction (me, get_default_dir (me));

  SCM att = me->get_property ("attachment");
      /*
       */
      if (!ly_c_pair_p (att))
	{
	  programming_error ("attachment is not a cons?!");
	  att = scm_cons (SCM_EOL, SCM_EOL);
	  me->set_property ("attachment", att);
	}
      
  Direction dir = LEFT;
  do 
    {
    
      if (!is_symbol (index_get_cell (att, dir)))
	{
	  SCM p = me->get_property ("extremity-function");
	  SCM res = ly_symbol2scm ("head");
	  
	  if (is_procedure (p))
	    res =  scm_call_2 (p, me->self_scm (), scm_int2num (dir));

	  if (is_symbol (res))
	    index_set_cell (att, dir, res);
	}
    }
  while (flip (&dir) != LEFT);

  check_slope (me);

  return att;
}


Real
Slur::get_boundary_notecolumn_y (Grob *me, Direction dir)
{
  SCM cols = me->get_property ("note-columns");

  if (!ly_c_pair_p (cols))
    {
      programming_error ("No note-columns in slur?");
      me->suicide ();
      return 0.0;
    }
  
  if (dir == LEFT)
    cols = scm_reverse (cols);
  
  Grob *col = unsmob_grob (ly_car (cols));
  Grob *common[] =
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
Slur::broken_trend_offset (Grob *me, Direction dir)
{
  /*
    A broken slur should maintain the same vertical trend
    the unbroken slur would have had.
  */
  Offset o;
  if (Spanner *mother =  dynamic_cast<Spanner*> (me->original_))
    {
      int k = broken_spanner_index (dynamic_cast<Spanner*> (me));
      int j = k + dir;
      if (j < 0 || j >= mother->broken_intos_.size ())
	return o;
      
      Grob *neighbour = mother->broken_intos_[j];      
      if (dir == RIGHT)
	neighbour->set_property ("direction",
				      me->get_property ("direction"));
      Real neighbour_y = get_boundary_notecolumn_y (neighbour, dir);
      Real y = get_boundary_notecolumn_y (me, -dir);
      int neighbour_cols = scm_ilength (neighbour->get_property ("note-columns"));
      int cols = scm_ilength (me->get_property ("note-columns"));
      o = Offset (0, (y*neighbour_cols + neighbour_y*cols) /
		  (cols + neighbour_cols));
    }
  return o;
}

/*
  COMMON is size-2 array with common refpoints.

UGH: this routine delivers offsets which are *not* relative to COMMON.

UGH,  we should take COMMON-Y as argument.
*/ 
Offset
Slur::get_attachment (Grob *me, Direction dir,
		      Grob **common) 
{
  SCM s = me->get_property ("attachment");
  if (!ly_c_pair_p (s) || !is_symbol (index_get_cell (s, dir)))
    {
      s = set_extremities (me);
    }
  
  SCM a = (dir == LEFT) ? ly_car (s) : ly_cdr (s);
  Spanner*sp = dynamic_cast<Spanner*> (me);
  String str = ly_symbol2string (a);
  
  Real staff_space = Staff_symbol_referencer::staff_space ((Grob*)me);
  Real hs = staff_space / 2.0;
  Offset o;
  
  Direction slurdir = to_dir (me->get_property ("direction"));
  
  Grob *stem = 0;
  if (Note_column::has_interface (sp->get_bound (dir)))
    {
      Grob * n =sp->get_bound (dir);
      stem = Note_column::get_stem (n);
      if (stem)
	{
	  Real x_extent;
	  Grob *head = Note_column::first_head (n);
	  if (head)
	    x_extent = head->extent (head, X_AXIS).length ();
	  else
	    x_extent = n->extent (n, X_AXIS).length ();

	  if (!head)
	    {
	      o = Offset (0, n->extent (n, Y_AXIS)[slurdir]);
	    }
	  else if (str == "head")
	    {
	      o = Offset (0, Stem::head_positions (stem)
			  [slurdir] * hs);
	      /*
		Default position is centered in X, on outer side of head Y
	       */
	      o += Offset (0.5 * x_extent,
			   0.5 * staff_space
			   * slurdir);
	    }
	  else if (str == "alongside-stem")
	    {
	      o = Offset (0, Stem::chord_start_y (stem));
	      /*
		Default position is on stem X, on outer side of head Y
	       */
	      o += Offset (x_extent * (1 + Stem::get_direction (stem)),
			   0.5 * staff_space
			   * slurdir);
	    }
	  else if (str == "stem")
	    {
	      o = Offset (0, Stem::stem_end_position (stem) * hs);
	      /*
		Default position is on stem X, at stem end Y
	       */
	      Real stem_thickness = Stem::thickness (stem);
	      o += Offset (0.5 *
			   x_extent * (1 + Stem::get_direction (stem))
			   - ((dir + 1)/2) * stem_thickness
			   + ((1 - slurdir)/2) * stem_thickness,
			   0);
	    }
	}
    }
  /*
    If we're not a note_column, we can't be anything but a loose-end.
    But if user has set (attachment . (stem . stem)), our string is
    stem, not loose-end.

    Hmm, maybe after-line-breaking should set this to loose-end?  */
  else // if (str == "loose-end")
    {
      SCM other_a = dir == LEFT ? ly_cdr (s) : ly_car (s);
      if (ly_symbol2string (other_a) != "loose-end")
	o = broken_trend_offset (me, dir);
    }

  SCM alist = me->get_property ("extremity-offset-alist");
  int stemdir = stem ? Stem::get_direction (stem) : 1;
  SCM l = scm_assoc
    (scm_list_n (a,
		  scm_int2num (stemdir * dir),
		  scm_int2num (slurdir * dir),
                  SCM_UNDEFINED), alist);

  if (l != SCM_BOOL_F)
    {
      Offset off = ly_scm2offset (ly_cdr (l)) * staff_space;
      off[X_AXIS] *= dir;
      off[Y_AXIS] *= slurdir;
      o += off;
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

  Offset off = ly_scm2offset (index_get_cell (me->get_property
					  ("attachment-offset"),
					  dir)) * staff_space;

  off[Y_AXIS] *= slurdir;
  o += off;
  return o;
}

Offset
Slur::encompass_offset (Grob*me,
			Grob* col,
			Grob **common) 
{
  Offset o;
  Grob* stem = unsmob_grob (col->get_property ("stem"));
  
  Direction dir = get_grob_direction (me);
  
  if (!stem)
    {
      programming_error ("No stem for note column?");
      o[X_AXIS] = col->relative_coordinate (common[X_AXIS], X_AXIS);
      o[Y_AXIS] = col->relative_coordinate (common[Y_AXIS], Y_AXIS);
      return o;  
    }
  Direction stem_dir = get_grob_direction (stem);
  o[X_AXIS] = stem->relative_coordinate (0, X_AXIS);

  /*
    Simply set x to middle of notehead
   */
  Real x_extent;
  if (Grob *head = Note_column::first_head (col))
    x_extent = head->extent (head, X_AXIS).length ();
  else
    x_extent = col->extent (col, X_AXIS).length ();
  o[X_AXIS] -= 0.5 * stem_dir * x_extent;

  if ((stem_dir == dir)
      && !stem->extent (stem, Y_AXIS).is_empty ())
    {
      o[Y_AXIS] = stem->extent (common[Y_AXIS], Y_AXIS)[dir];
    }
  else
    {
      o[Y_AXIS] = col->extent (common[Y_AXIS], Y_AXIS)[dir];
    }

  /*
   leave a gap: slur mustn't touch head/stem
   */
  o[Y_AXIS] += dir * robust_scm2double (me->get_property ("y-free"), 0) *
    1.0;
  return o;
}

Array<Offset>
Slur::get_encompass_offsets (Grob *me)
{
  Spanner*sp = dynamic_cast<Spanner*> (me);
  SCM eltlist = me->get_property ("note-columns");
  Grob *common[] = {common_refpoint_of_list (eltlist, me, X_AXIS),
		    common_refpoint_of_list (eltlist, me, Y_AXIS)};


  common[X_AXIS] = common[X_AXIS]->common_refpoint (sp->get_bound (RIGHT), X_AXIS);
  common[X_AXIS] = common[X_AXIS]->common_refpoint (sp->get_bound (LEFT), X_AXIS);
  
  Link_array<Grob>  encompasses = ly_scm2grobs (eltlist);
  Array<Offset> offsets;

  Offset origin (me->relative_coordinate (common[X_AXIS], X_AXIS),
		 me->relative_coordinate (common[Y_AXIS], Y_AXIS)); 

  int first = 1;
  int last = encompasses.size () - 2;

  offsets.push (get_attachment (me, LEFT, common));

  /*
    left is broken edge
  */
  if (encompasses[0] != sp->get_bound (LEFT))
    {
      first--;

      // ?
      offsets[0][Y_AXIS] -=
	encompasses[0]->relative_coordinate (common[Y_AXIS], Y_AXIS) 
	- me->relative_coordinate (common[Y_AXIS], Y_AXIS); 
    }

  /*
    right is broken edge
  */
  if (encompasses.top () != sp->get_bound (RIGHT))
    {
      last++;
    }

  for (int i = first; i <= last; i++)
    {
      Offset o (encompass_offset (me, encompasses[i], common));
      offsets.push (o - origin);
    }

  offsets.push (Offset (sp->spanner_length (), 0) + get_attachment (me, RIGHT,common));

  if (encompasses[0] != sp->get_bound (LEFT))
    {
      offsets.top ()[Y_AXIS] -= encompasses.top ()->relative_coordinate (common[Y_AXIS], Y_AXIS) 
	- me->relative_coordinate (common[Y_AXIS], Y_AXIS);
    }

  return offsets;
}




/*
  ugh ?
 */
MAKE_SCHEME_CALLBACK (Slur, height, 2);
SCM
Slur::height (SCM smob, SCM ax)
{
  Axis a = (Axis)ly_scm2int (ax);
  Grob * me = unsmob_grob (smob);
  assert (a == Y_AXIS);

  SCM mol = me->get_uncached_stencil ();
  Interval ext;
  if (Stencil * m = unsmob_stencil (mol))
    ext = m->extent (a);
  return ly_interval2scm (ext);
}

/*
  Ugh should have dash-length + dash-period
 */
MAKE_SCHEME_CALLBACK (Slur, print,1);
SCM
Slur::print (SCM smob)
{
  Grob * me = unsmob_grob (smob);
  if (!scm_ilength (me->get_property ("note-columns")))
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real base_thick = robust_scm2double (me->get_property ("thickness"), 1);
  Real thick = base_thick * Staff_symbol_referencer::line_thickness (me);

  Real ss = Staff_symbol_referencer::staff_space (me);
  Bezier one = get_curve (me);

  // get_curve may suicide
  if (!scm_ilength (me->get_property ("note-columns")))
    return SCM_EOL;

  Stencil a;

  /*
    TODO: replace dashed with generic property.
   */
  SCM d =  me->get_property ("dashed");
  if (is_number (d))
    a = Lookup::dashed_slur (one, thick, thick * robust_scm2double (d, 0));
  else
    a = Lookup::slur (one, get_grob_direction (me) * base_thick * ss / 10.0,
		      thick);

  return a.smobbed_copy ();
}

void
Slur::set_control_points (Grob*me)
{
  Real staff_space = Staff_symbol_referencer::staff_space ((Grob*)me);

  SCM details = me->get_property ("details");
  SCM h_inf_scm = me->get_property ("height-limit");
  SCM r_0_scm = me->get_property ("ratio");

  Real r_0 = robust_scm2double (r_0_scm, 1);
  Real h_inf = staff_space * ly_scm2double (h_inf_scm);
  
  Slur_bezier_bow bb (get_encompass_offsets (me),
		      get_grob_direction (me),
		      h_inf, r_0);


  if (bb.fit_factor () > 1.0)
    {
      Real length = bb.curve_.control_[3][X_AXIS]; 
      Real default_height = slur_height (length, h_inf, r_0);

      SCM ssb = me->get_property ("beautiful");
      Real sb = 0;
      if (is_number (ssb))
	sb = ly_scm2double (ssb);

      bb.minimise_enclosed_area (sb, details);
      SCM sbf = scm_assq (ly_symbol2scm ("force-blowfit"), details);
      Real bff = 1.0;
      if (ly_c_pair_p (sbf) && is_number (ly_cdr (sbf)))
	  bff = ly_scm2double (ly_cdr (sbf));

      bb.curve_.control_[1][Y_AXIS] *= bff;
      bb.curve_.control_[2][Y_AXIS] *= bff;
      bb.blow_fit ();

      
      Real beautiful = length * default_height * sb;
      Real area = bb.get_enclosed_area ();
      
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
      controls = scm_cons (ly_offset2scm (b.control_[i]), controls);
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

  me->set_property ("control-points", controls);
}
  
Bezier
Slur::get_curve (Grob*me) 
{
  Bezier b;
  int i = 0;

  SCM attach = me->get_property ("attachment");
  if (!ly_c_pair_p (attach))
    attach = set_extremities (me);

  
  if (!get_grob_direction (me)
      || ! is_symbol (index_get_cell (attach, LEFT))
      || ! is_symbol (index_get_cell (attach, RIGHT)))
    set_extremities (me);
  
  if (!ly_c_pair_p (me->get_property ("control-points")))
    set_control_points (me);

  // set_control_points may suicide
  if (!scm_ilength (me->get_property ("note-columns")))
    return b;

  for (SCM s= me->get_property ("control-points"); s != SCM_EOL; s = ly_cdr (s))
    {
      b.control_[i] = ly_scm2offset (ly_car (s));
      i++;
    }

  Array<Offset> enc (get_encompass_offsets (me));
  Direction dir = get_grob_direction (me);
  
  Real x1 = enc[0][X_AXIS];
  Real x2 = enc.top ()[X_AXIS];

  Real off = 0.0;
  for (int i=1; i < enc.size ()-1; i++)
    {
      Real x = enc[i][X_AXIS];
      if (x > x1 && x <x2)
	{
	  Real y = b.get_other_coordinate (X_AXIS, x);
	  off = off >? dir * (enc[i][Y_AXIS] - y);
	}
    }
  b.translate (Offset (0, dir * off));
  return b;
}




ADD_INTERFACE (Slur,"slur-interface",
  "A slur",
  "attachment attachment-offset beautiful control-points dashed details de-uglify-parameters direction extremity-function extremity-offset-alist height-limit note-columns ratio slope-limit thickness y-free");

