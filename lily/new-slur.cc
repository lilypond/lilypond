/*
  slur.cc -- implement score based Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include <math.h>

#include "main.hh"
#include "font-interface.hh"
#include "text-item.hh"
#include "directional-element-interface.hh"
#include "group-interface.hh"
#include "lily-guile.hh"
#include "lookup.hh"
#include "note-column.hh"
#include "output-def.hh"
#include "rod.hh"
#include "slur-bezier-bow.hh"
#include "slur.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "stem.hh"
#include "stencil.hh"
#include "warn.hh"
#include "beam.hh"

/*
  TODO:

  - curve around flag for y coordinate
  - better scoring.
  - short-cut: try a smaller region first.
  - collisions with accidentals
  - collisions with scripts (staccato!)
  - 
*/
struct Encompass_info
{
  Real x_;
  Real stem_;
  Real head_;
  Encompass_info ()
  {
    x_ = 0.0;
    stem_ = 0.0;
    head_ = 0.0;
  }
};

struct Bound_info
{
  Box stem_extent_;
  Direction stem_dir_;
  Grob *bound_;
  Grob *note_column_;
  Grob *slur_head_;
  Grob *staff_;
  Grob *stem_;
  Interval slur_head_extent_;
  Real neighbor_y_;
  Real staff_space_;
  
  Bound_info ()
  {
    stem_ = 0;
    neighbor_y_ = 0;
    staff_ = 0;
    slur_head_ = 0;
    stem_dir_ = CENTER;
    note_column_ = 0;
  }
};

/*
  TODO: put in details property.
*/
struct Slur_score_parameters
{
  int SLUR_REGION_SIZE;
  Real HEAD_ENCOMPASS_PENALTY;
  Real STEM_ENCOMPASS_PENALTY;
  Real CLOSENESS_FACTOR;
  Real EDGE_ATTRACTION_FACTOR;
  Real SAME_SLOPE_PENALTY;
  Real STEEPER_SLOPE_FACTOR;
  Real NON_HORIZONTAL_PENALTY;
  Real HEAD_STRICT_FREE_SPACE;
  Real MAX_SLOPE;
  Real MAX_SLOPE_FACTOR;
  Real FREE_HEAD_DISTANCE;
  Slur_score_parameters ();
};

void
init_score_param (Slur_score_parameters *score_param)
{
  score_param->SLUR_REGION_SIZE = 5;
  score_param->HEAD_ENCOMPASS_PENALTY = 1000.0;
  score_param->STEM_ENCOMPASS_PENALTY = 30.0;
  score_param->CLOSENESS_FACTOR = 10;
  score_param->EDGE_ATTRACTION_FACTOR = 4;
  score_param->SAME_SLOPE_PENALTY = 20;
  score_param->STEEPER_SLOPE_FACTOR = 50;
  score_param->NON_HORIZONTAL_PENALTY = 15;
  score_param->HEAD_STRICT_FREE_SPACE = 0.2;
  score_param->MAX_SLOPE = 1.1;
  score_param->MAX_SLOPE_FACTOR = 10;
  score_param->FREE_HEAD_DISTANCE = 0.3;
}

Slur_score_parameters::Slur_score_parameters()
{
  init_score_param (this);
}

#define DEBUG_SLUR_QUANTING 1

struct Slur_score
{
  Drul_array<Offset> attachment_;
  Real score_;
  Bezier curve_;

#if DEBUG_SLUR_QUANTING
  String score_card_;
#endif

  Slur_score()
  {
    score_ = 0.0;
  }
};

class New_slur
{
public:
  static void add_column (Grob *me, Grob *col);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static void score_slopes (Grob *me, Grob *common[],
			    Slur_score_parameters *score_param,
			    Drul_array<Bound_info>,
			    Drul_array<Offset> base_attach,
			    Array<Slur_score> *scores);

  static void score_edges (Grob *me, Grob *common[],
			   Slur_score_parameters *score_param,
			   Drul_array<Bound_info> extremes,
			   Drul_array<Offset> base_attach,
			   Array<Slur_score> *scores);
  static void score_encompass (Grob *me, Grob *common[],
			       Slur_score_parameters*,
			       Drul_array<Bound_info>,
			       Drul_array<Offset>, Array<Slur_score> *scores);
  static Bezier avoid_staff_line (Grob *me, Grob **common,
				  Drul_array<Bound_info> extremes,
				  Bezier bez);

  static Encompass_info get_encompass_info (Grob *me,
					    Grob *col,
					    Grob **common);
  static void set_interface (Grob *);
  static bool  has_interface (Grob *);
  static Bezier get_curve (Grob *me);
  static Bezier get_bezier (Grob *me, Drul_array<Offset>, Real, Real);
  static Direction get_default_dir (Grob *me);
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM,SCM));

  static void set_end_points (Grob *);
  static Real get_boundary_notecolumn_y (Grob *me, Direction dir);
  static Real broken_trend_y (Grob *me, Grob **, Direction dir);
  static void set_control_points (Grob *me);
  static Drul_array<Bound_info> get_bound_info (Spanner *me, Grob **common);

  static void generate_curves (Grob *me,
			       Grob *common[],
			       Drul_array<Bound_info> extremes,
			       Drul_array<Offset> base_attach,
			       Array<Slur_score> *scores);
  static Array<Slur_score> enumerate_attachments
  (Grob *me, Grob *common[], Slur_score_parameters *score_param,
   Drul_array<Bound_info> extremes,
   Drul_array<Offset> base_attachment, Drul_array<Real> end_ys);
  static Drul_array<Offset> get_base_attachments
  (Spanner *sp, Grob **common, Drul_array<Bound_info> extremes);
  static Drul_array<Real> get_y_attachment_range
  (Spanner *sp, Grob **common, Drul_array<Bound_info> extremes,
   Drul_array<Offset> base_attachment);
};

/* HDIR indicates the direction for the slur.  */
Real
New_slur::broken_trend_y (Grob *me, Grob **common, Direction hdir)
{
  /* A broken slur should maintain the same vertical trend
    the unbroken slur would have had.  */
  Real by = 0.0;
  if (Spanner *mother = dynamic_cast<Spanner*> (me->original_))
    {
      int k = broken_spanner_index (dynamic_cast<Spanner*> (me));
      int j = k + hdir;
      if (j < 0 || j >= mother->broken_intos_.size ())
	return by;

      Grob *neighbor = mother->broken_intos_[j];
      if (hdir == RIGHT)
	neighbor->set_property ("direction",
				me->get_property ("direction"));

      Spanner *common_mother
	= dynamic_cast<Spanner*> (common[Y_AXIS]->original_);
      int common_k
	= broken_spanner_index (dynamic_cast<Spanner*> (common[Y_AXIS]));
      int common_j = common_k + hdir;

      if (common_j < 0 || common_j >= common_mother->broken_intos_.size())
	return by;

      Grob *common_next_system = common_mother->broken_intos_[common_j];
      Link_array<Grob> neighbor_cols
	= Pointer_group_interface__extract_grobs (neighbor, (Grob *)0,
						  "note-columns");

      Grob *neighbor_col
	= (hdir == RIGHT) ? neighbor_cols[0] : neighbor_cols.top ();
      Grob *neighbor_common
	= common_next_system->common_refpoint (neighbor_col, Y_AXIS);

      Direction vdir = get_grob_direction (me);
      Real neighbor_y
	= neighbor_col->extent (neighbor_common, Y_AXIS)
	.linear_combination (int(neighbor_cols.size()==1 ? CENTER : vdir))
	- common_next_system->relative_coordinate (neighbor_common, Y_AXIS);

      Link_array<Grob> my_cols
	= Pointer_group_interface__extract_grobs (me, (Grob *)0,
						  "note-columns");

      Grob *extreme_col = (hdir == RIGHT) ? my_cols.top() : my_cols[0];
      Real y = extreme_col->extent (common[Y_AXIS], Y_AXIS)
	.linear_combination (int ((my_cols.size() == 1) ? CENTER : vdir));
      by = (y*neighbor_cols.size() + neighbor_y*my_cols.size()) /
	(my_cols.size() + neighbor_cols.size());
    }
  return by;
}

void
New_slur::set_interface (Grob*me)
{
  /* Copy to mutable list. */
  me->set_property ("attachment",
		    ly_deep_copy (me->get_property ("attachment")));
}

void
New_slur::add_column (Grob*me, Grob*n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-columns"), n);
  me->add_dependency (n);

  add_bound_item (dynamic_cast<Spanner*> (me), dynamic_cast<Item*> (n));
}

Encompass_info
New_slur::get_encompass_info (Grob *me,
			      Grob *col,
			      Grob **common)
{
  Grob* stem = unsmob_grob (col->get_property ("stem"));

  Encompass_info ei;

  Direction dir = get_grob_direction (me);

  if (!stem)
    {
      programming_error ("No stem for note column?");
      ei.x_ = col->relative_coordinate (common[X_AXIS], X_AXIS);
      ei.head_ = ei.stem_ = col->extent (common[Y_AXIS],
					 Y_AXIS)[get_grob_direction (me)];
      return ei;
    }
  Direction stem_dir = get_grob_direction (stem);

  if (Grob *head = Note_column::first_head (col))
    ei.x_ = head->extent (common[X_AXIS], X_AXIS).center ();
  else
    ei.x_ = col->extent (common[X_AXIS], X_AXIS).center ();

  Grob * h = Stem::extremal_heads (stem)[Direction (dir)];
  if (!h)
    {
      ei.head_ = ei.stem_ = col->extent (common[Y_AXIS], Y_AXIS)[dir];
      return ei;
    }

  ei.head_ = h->extent (common[Y_AXIS], Y_AXIS)[dir];

  if ((stem_dir == dir)
      && !stem->extent (stem, Y_AXIS).is_empty ())
    {
      ei.stem_ = stem->extent (common[Y_AXIS], Y_AXIS)[dir];
      if (Grob * b = Stem::get_beam (stem))
	ei.stem_ += stem_dir * 0.5 * Beam::get_thickness (b);
      ei.x_  = stem->extent (common[X_AXIS], X_AXIS).center ();
    }
  else
    ei.stem_ = ei.head_;

  return ei;
}


Direction
New_slur::get_default_dir (Grob*me)
{
  Link_array<Grob> encompasses
    = Pointer_group_interface__extract_grobs (me, (Grob*) 0, "note-columns");

  Direction d = DOWN;
  for (int i= 0; i < encompasses.size (); i ++)
    {
      if (Note_column::dir (encompasses[i]) < 0)
	{
	  d = UP;
	  break;
	}
    }
  return d;
}

MAKE_SCHEME_CALLBACK (New_slur, after_line_breaking,1);
SCM
New_slur::after_line_breaking (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner*> (unsmob_grob (smob));
  if (!scm_ilength (me->get_property ("note-columns")))
    {
      me->suicide ();
      return SCM_UNSPECIFIED;
    }

  if (!get_grob_direction (me))
    set_grob_direction (me, get_default_dir (me));

  set_end_points (me);

  return SCM_UNSPECIFIED;
}

Bezier
New_slur::get_bezier (Grob *me, Drul_array<Offset> extremes,
		      Real r_0,
		      Real h_inf)
{
  Array<Offset> encompasses;
  encompasses.push (extremes[LEFT]);
  encompasses.push (extremes[RIGHT]);

  Slur_bezier_bow bb (encompasses,
		      get_grob_direction (me), h_inf, r_0);

  return bb.get_bezier ();
}


Drul_array<Bound_info>
New_slur::get_bound_info (Spanner* me,
			  Grob **common)
{
  Drul_array<Bound_info> extremes;
  Direction d = LEFT;
  Direction dir = get_grob_direction (me);

  do {
    extremes[d].bound_ = me->get_bound (d);

    if (Note_column::has_interface (extremes[d].bound_))
      {
	extremes[d].note_column_ = extremes[d].bound_;
	extremes[d].stem_ = Note_column::get_stem (extremes[d].note_column_);
	extremes[d].stem_dir_ = get_grob_direction (extremes[d].stem_);
	extremes[d].stem_extent_[X_AXIS] = extremes[d].stem_->extent (common[X_AXIS], X_AXIS);
	extremes[d].stem_extent_[Y_AXIS] = extremes[d].stem_->extent (common[Y_AXIS], Y_AXIS);
	extremes[d].slur_head_ = Stem::extremal_heads (extremes[d].stem_)[dir];
	extremes[d].slur_head_extent_ = extremes[d].slur_head_->extent (common[X_AXIS], X_AXIS);
	extremes[d].staff_ = Staff_symbol_referencer::get_staff_symbol (extremes[d].slur_head_);
	extremes[d].staff_space_ = Staff_symbol_referencer::staff_space (extremes[d].slur_head_);
      }
    else
      {
	extremes[d].neighbor_y_ = broken_trend_y (me, common, d);
      }
  } while (flip (&d) != LEFT);

  return extremes;
}

void
New_slur::set_end_points (Grob *me)
{
  Link_array<Grob> columns
    = Pointer_group_interface__extract_grobs (me, (Grob *)0, "note-columns");
  Slur_score_parameters params;
  if (columns.is_empty ())
    {
      me->suicide ();
      return;
    }

  SCM eltlist = me->get_property ("note-columns");
  Grob *common[] = {common_refpoint_of_list (eltlist, me, X_AXIS),
		    common_refpoint_of_list (eltlist, me, Y_AXIS)};


  Spanner *sp = dynamic_cast<Spanner*> (me);
  common[X_AXIS] = common[X_AXIS]->common_refpoint (sp->get_bound (RIGHT),
						    X_AXIS);
  common[X_AXIS] = common[X_AXIS]->common_refpoint (sp->get_bound (LEFT),
						    X_AXIS);
  Drul_array<Bound_info> extremes = get_bound_info (sp, common);
  Drul_array<Offset> base_attachment
    = get_base_attachments (sp, common, extremes);
  Drul_array<Real> end_ys
    = get_y_attachment_range (sp, common, extremes, base_attachment);
  Array<Slur_score> scores = enumerate_attachments (me, common, &params,
						    extremes, base_attachment,
						    end_ys);

  generate_curves (me, common, extremes, base_attachment, &scores);
  score_edges (me, common, &params,extremes, base_attachment, &scores);
  score_slopes (me, common, &params,extremes, base_attachment, &scores);
  score_encompass (me, common, &params,extremes, base_attachment, &scores);
  
  Real opt = 1e6;
  int opt_idx = 0;
  // why backwards?
  for (int i = scores.size (); i--;)
    {
      if (scores[i].score_  < opt)
	{
	  opt = scores[i].score_;
	  opt_idx = i;
	}
    }

#if DEBUG_SLUR_QUANTING
  SCM inspect_quants = me->get_property ("inspect-quants");
  if (to_boolean (me->get_paper ()
		  ->lookup_variable (ly_symbol2scm ("debug-slur-quanting")))
      && ly_c_pair_p (inspect_quants))
    {
      Drul_array<Real> ins = ly_scm2interval (inspect_quants);
      int i = 0;

      Real mindist = 1e6;
      for (; i < scores.size (); i ++)
	{
	  Real d =fabs (scores[i].attachment_[LEFT][Y_AXIS] - ins[LEFT])
	    + fabs (scores[i].attachment_[RIGHT][Y_AXIS] - ins[RIGHT]);
	  if (d < mindist)
	    {
	      opt_idx = i;
	      mindist= d;
	    }
	}
      if (mindist > 1e5)
	programming_error ("Could not find quant.");
    }
  scores[opt_idx].score_card_ += to_string ("i%d", opt_idx);

  // debug quanting
  me->set_property ("quant-score",
		    scm_makfrom0str (scores[opt_idx].score_card_.to_str0 ()));
#endif
  
  Bezier b
    = avoid_staff_line  (me,common, extremes, scores[opt_idx].curve_);
  
  SCM controls = SCM_EOL;
  for (int i = 4; i--;)
    {
      Offset o = b.control_[i] -
	Offset (me->relative_coordinate (common[X_AXIS], X_AXIS),
		me->relative_coordinate (common[Y_AXIS], Y_AXIS));

      controls = scm_cons (ly_offset2scm (o), controls);
    }

  me->set_property ("control-points", controls);
}


Drul_array<Real>
New_slur::get_y_attachment_range (Spanner*me,
				  Grob **common,
				  Drul_array<Bound_info> extremes,
				  Drul_array<Offset> base_attachment)
{
  Drul_array<Real> end_ys;
  Direction dir = get_grob_direction (me);
  Direction d = LEFT;
  do
    {
      if (extremes[d].note_column_)
	{
	  end_ys[d] = dir
	    * ((dir * (base_attachment[d][Y_AXIS] + 4.0 * dir))
	       >? (dir * (dir + extremes[d].note_column_->extent(common[Y_AXIS],
								 Y_AXIS)[dir]))
	       >? (dir * base_attachment[-d][Y_AXIS]));
	}
      else
      end_ys[d] = extremes[d].neighbor_y_ + 4.0 * dir;
    }
  while (flip (&d) != LEFT);

  return end_ys;
}

Drul_array<Offset>
New_slur::get_base_attachments (Spanner *me,
				Grob **common, Drul_array<Bound_info> extremes)
{
  Link_array<Grob> columns
    = Pointer_group_interface__extract_grobs (me, (Grob *)0, "note-columns");
  Drul_array<Offset> base_attachment;
  Slur_score_parameters params;
  Real staff_space = Staff_symbol_referencer::staff_space ((Grob*)me);
  Direction dir = get_grob_direction (me);
  Direction d = LEFT;
  do
    {
      Grob *stem = extremes[d].stem_;
      Grob *head = extremes[d].slur_head_;

      Real x, y;
      if (!extremes[d].note_column_)
	{
	  y = extremes[d].neighbor_y_;
	  if (d== RIGHT)
	    x = extremes[d].bound_->extent (common[X_AXIS], X_AXIS)[d];
	  else
	    x = me->get_broken_left_end_align ();
	}
      else
	{
	  if (stem
	      && extremes[d].stem_dir_ == dir
	      && Stem::get_beaming (stem, -d)
	      && columns.size () > 2
	      )
	    y = extremes[d].stem_extent_[Y_AXIS][dir];
	  else if (head)
	    y = head->extent (common[Y_AXIS], Y_AXIS)[dir];
	  y += dir * 0.5 * staff_space;

	  if (stem)
	    {
	      SCM scripts = stem->get_property ((dir == UP
						 ? "script-up"
						 : "script-down"));

	      /* FIXME: number of scripts, height, priority?
	         articulation scripts should be inside slur, other,
	         such as dynamics, pedals, fingering, should be
	         outside.  */
	      if (!SCM_NULLP (scripts))
		y += dir * staff_space;
	    }

	  Real pos
	    = (y - extremes[d].staff_->relative_coordinate (common[Y_AXIS],
							    Y_AXIS))
	    * 2.0 / Staff_symbol::staff_space (extremes[d].staff_);

	  /* start off staffline. */
	  if (fabs (pos - round (pos)) < 0.2
	      && Staff_symbol_referencer::on_staffline (head, (int) rint (pos))
	      && Staff_symbol_referencer::line_count (head) - 1 >= rint (pos)
	      )
	    // TODO: calc from slur thick & line thick, parameter.	    
	    y += 1.5 * staff_space * dir / 10;

	  Grob * fh = Note_column::first_head (extremes[d].note_column_);
	  x = fh->extent (common[X_AXIS], X_AXIS).linear_combination (CENTER);
	}
      base_attachment[d] = Offset (x, y);

    } while (flip (&d) != LEFT);

  return base_attachment;
}

void
New_slur::generate_curves (Grob *me, Grob **common,
			   Drul_array<Bound_info> extremes,
			   Drul_array<Offset>,
			   Array<Slur_score> *scores)
{
  (void) common;
  (void) extremes;
  Real staff_space = Staff_symbol_referencer::staff_space ((Grob *) me);
  Real r_0 = robust_scm2double (me->get_property ("ratio"), 0.33);
  Real h_inf = staff_space * ly_scm2double (me->get_property ("height-limit"));
  for (int i = 0; i < scores->size(); i++)
    {
      Bezier bez= get_bezier (me, (*scores)[i].attachment_, r_0, h_inf);
      (*scores)[i].curve_ = bez;
    }
}

Bezier
New_slur::avoid_staff_line (Grob *me, Grob **common,
			    Drul_array<Bound_info> extremes,
			    Bezier bez)
{
  Offset horiz (1,0);
  Array<Real> ts  = bez.solve_derivative (horiz);
  Real lt =  me->get_paper ()->get_dimension (ly_symbol2scm ("linethickness"));
  Real thick = robust_scm2double (me->get_property ("thickness"), 1.0) *  lt;

  /* TODO: handle case of broken slur.  */
  if (!ts.is_empty ()
      && (extremes[LEFT].staff_ == extremes[RIGHT].staff_)
      && extremes[LEFT].staff_ && extremes[RIGHT].staff_)
    {
      Real y = bez.curve_point (ts[0])[Y_AXIS];

      Grob *staff = extremes[LEFT].staff_;

      Real staff_space = extremes[LEFT].staff_space_;
      Real p = 2 * (y - staff->relative_coordinate (common[Y_AXIS], Y_AXIS))
	/ staff_space;

      Real distance = fabs (round (p) - p);	//  in halfspaces
      if (distance < 4 * thick
	  && (int) fabs (round (p))
	  <= 2 * Staff_symbol_referencer::staff_radius (staff) + 0.1
	  && (int (fabs (round (p))) % 2
	      != Staff_symbol_referencer::line_count (staff) % 2))
	{
	  // TODO: parameter
	  Real newp = round (p) + get_grob_direction (me) * 5 * thick;
	  Real dy = (newp - p) * staff_space / 2.0;
	  bez.translate (Offset (0, dy));
	}
    }
  return bez;
}

Array<Slur_score>
New_slur::enumerate_attachments (Grob *me, Grob **,
				 Slur_score_parameters *score_param,
				 Drul_array<Bound_info> extremes,
				 Drul_array<Offset> base_attachment,
				 Drul_array<Real> end_ys)
{
  /*ugh.   */
  Array<Slur_score> scores;

  Direction dir = get_grob_direction (me);
  Real staff_space = Staff_symbol_referencer::staff_space ((Grob *)me);

  Drul_array<Offset> os;
  os[LEFT] = base_attachment[LEFT];
  Real minimum_length = staff_space
    * robust_scm2double (me->get_property ("minimum-length"), 2.0);

  for (int i = 0; dir * os[LEFT][Y_AXIS] <= dir * end_ys[LEFT]; i++)
    {
      os[RIGHT] = base_attachment[RIGHT];
      for (int j = 0; dir * os[RIGHT][Y_AXIS] <= dir * end_ys[RIGHT]; j++)
	{
	  Slur_score s;
	  Direction d = LEFT;
	  Drul_array<bool> attach_to_stem (false, false);
	  do {  
	    os[d][X_AXIS] = base_attachment[d][X_AXIS];
	    if (extremes[d].stem_
		&& !Stem::is_invisible (extremes[d].stem_)
		&& extremes[d].stem_dir_ == dir
		&& dir == -d)
	      {
		if (extremes[d].stem_extent_[Y_AXIS].contains (os[d][Y_AXIS]))
		  {
		    os[d][X_AXIS] =  extremes[d].slur_head_extent_[-d]
		      - d * 0.3;
		    attach_to_stem[d] = true;
		  }
		else if (dir *extremes[d].stem_extent_[Y_AXIS][dir] < dir * os[d][Y_AXIS])
		  {
		    os[d][X_AXIS] = extremes[d].stem_extent_[X_AXIS].center();
		  }
	      }
	  } while (flip (&d) != LEFT);

	  Offset dz;	  
	  dz = os[RIGHT] - os[LEFT];
	  if (dz[X_AXIS] < minimum_length
	      || fabs (dz[Y_AXIS] / dz[X_AXIS]) > score_param->MAX_SLOPE
	      )
	    {
	      do
		{
		 if (extremes[d].slur_head_)
		   {
		     os[d][X_AXIS] = extremes[d].slur_head_extent_.center ();
		     attach_to_stem[d] = false;
		   }
	      }
	      while (flip (&d) != LEFT);
	    }

	  dz = os[RIGHT] - os[LEFT];
	  do {
	    if (extremes[d].slur_head_
		&& !attach_to_stem[d])
	      {
		/*
		  horizontally move tilted slurs a little. Move more
		  for bigger tilts.

		  TODO: parameter
		 */
		os[d][X_AXIS] -=
		  dir * extremes[d].slur_head_extent_.length () * sin (dz.arg  ()) / 3;
	      }
	  } while (flip (&d) != LEFT);
	  
	  s.attachment_ = os;
	  scores.push (s);

	  os[RIGHT][Y_AXIS] += dir * staff_space / 2;
	}

      os[LEFT][Y_AXIS] += dir * staff_space / 2;
    }

  return scores;
}

void
New_slur::score_encompass (Grob *me, Grob *common[],
			   Slur_score_parameters *score_param,
			   Drul_array<Bound_info> ,
			   Drul_array<Offset> ,
			   Array<Slur_score> * scores)
{
  Link_array<Grob> encompasses
    = Pointer_group_interface__extract_grobs (me, (Grob *)0, "note-columns");
  Direction dir = get_grob_direction (me);

  Array<Encompass_info> infos;

  for (int i = 0; i < encompasses.size(); i++)
    infos.push (get_encompass_info (me, encompasses[i], common));

  for (int i = 0; i < scores->size (); i++)
    {
      Bezier const &bez (scores->elem (i).curve_);
      Real demerit = 0.0;
      for (int j = 0; j < infos.size(); j++)
	{
	  Real x = infos[j].x_;

	  bool l_edge = j==0;
	  bool r_edge = j==infos.size()-1;
	  bool edge =  l_edge || r_edge;
  
	  if (!(x < scores->elem (i).attachment_[RIGHT][X_AXIS]
		&& x > scores->elem (i).attachment_[LEFT][X_AXIS]))
	    continue;
	
	  Real y = bez.get_other_coordinate (X_AXIS, x);
	  if (!edge)
	    {
	      Real head_dy = (y - infos[j].head_);
	      if (dir * head_dy < 0)
		demerit += score_param->HEAD_ENCOMPASS_PENALTY;
	      else
		{
		  Real hd = (head_dy)
		    ? (1 / fabs (head_dy) - 1 / score_param->FREE_HEAD_DISTANCE)
		    : score_param->HEAD_ENCOMPASS_PENALTY;
		  hd = (hd >? 0)<? score_param->HEAD_ENCOMPASS_PENALTY;

		  demerit += hd;	
		}
	    }	  

	  if (edge
	      && ((l_edge && dir == UP)
		  || (r_edge && dir == DOWN))
	      && (dir * (y - infos[j].stem_) < 0))
	    demerit += score_param->STEM_ENCOMPASS_PENALTY;
	  else if (!edge)
	    {
	      Interval ext;
	      ext.add_point (infos[j].stem_);
	      ext.add_point (infos[j].head_);

	      demerit += - score_param->CLOSENESS_FACTOR * (dir * (y - (ext[dir] + dir * score_param->FREE_HEAD_DISTANCE)) <? 0) /
		infos.size ();
	    }
	}

#if DEBUG_SLUR_QUANTING
      (*scores)[i].score_card_ += to_string ("C%.2f", demerit);
#endif

      (*scores)[i].score_ += demerit;
    }
}

void
New_slur::score_edges (Grob *me, Grob **,
		       Slur_score_parameters * score_param,
		       Drul_array<Bound_info> extremes,
		       Drul_array<Offset> base_attach,
		       Array<Slur_score> *scores)
{
  Direction dir = get_grob_direction (me);

  for (int i = 0; i < scores->size (); i++)
    {

      Direction d = LEFT;
      do {
	Real y = scores->elem (i).attachment_[d][Y_AXIS];
	Real dy = fabs (y - base_attach[d][Y_AXIS]);
	
	Real factor = score_param->EDGE_ATTRACTION_FACTOR;
	Real demerit = factor * dy;
 	if (extremes[d].stem_
 	    && extremes[d].stem_dir_ == dir
 	    && !Stem::get_beaming (extremes[d].stem_, -d)
 	    )
 	  demerit /= 5;
	
	(*scores)[i].score_ += demerit;
#if DEBUG_SLUR_QUANTING
	(*scores)[i].score_card_ += to_string ("E%.2f", demerit);
#endif
      } while (flip (&d) != LEFT);
    }
}

void
New_slur::score_slopes (Grob *me, Grob *common[],
			Slur_score_parameters*score_param,
			Drul_array<Bound_info> extremes,
			Drul_array<Offset> base_attach,
			Array<Slur_score> * scores)
{
  Drul_array<Real> ys;

  Direction d = LEFT;
  do {
    if (extremes[d].slur_head_)
      ys[d] = extremes[d].slur_head_ ->relative_coordinate (common[Y_AXIS],
							    Y_AXIS);
    else
      ys[d] = extremes[d].neighbor_y_;
  } while (flip (&d) != LEFT);

  bool has_beams
    = (extremes[LEFT].stem_ && Stem::get_beam (extremes[LEFT].stem_))
    || (extremes[RIGHT].stem_ && Stem::get_beam (extremes[RIGHT].stem_));

  Real dy = ys[RIGHT] - ys[LEFT];
  for (int i = 0; i < scores->size (); i++)
    {
      Offset slur_dz = (*scores)[i].attachment_[RIGHT]
	-  (*scores)[i].attachment_[LEFT];
      Real slur_dy = slur_dz[Y_AXIS]; 
      Real demerit = 0.0;
      if (!has_beams)
	/* 0.2: account for staffline offset. */
	demerit += score_param->STEEPER_SLOPE_FACTOR * ((fabs (slur_dy) - (fabs (dy) + 0.2)) >? 0); 

      demerit += ((fabs (slur_dy/slur_dz[X_AXIS]) - score_param->MAX_SLOPE)>?0)  * score_param->MAX_SLOPE_FACTOR;
      
      if (sign (dy) == 0 &&
	  sign (slur_dy) != 0)
	demerit += score_param->NON_HORIZONTAL_PENALTY;

      if (sign (dy)
	  && sign (slur_dy)
	  && sign (slur_dy) != sign (dy))
	demerit += has_beams
	  ? score_param->SAME_SLOPE_PENALTY / 10
	  : score_param->SAME_SLOPE_PENALTY;

#if DEBUG_SLUR_QUANTING
      (*scores)[i].score_card_ += to_string ("S%.2f",d);
#endif
      (*scores)[i].score_ += demerit;
    }
}


Bezier
New_slur::get_curve (Grob*me)
{
  Bezier b;
  int i = 0;
  for (SCM s = me->get_property ("control-points"); s != SCM_EOL;
       s = ly_cdr (s))
    b.control_[i++] = ly_scm2offset (ly_car (s));

  return b;
}


MAKE_SCHEME_CALLBACK (New_slur, height, 2);
SCM
New_slur::height (SCM smob, SCM ax)
{
  Axis a = (Axis)ly_scm2int (ax);
  Grob *me = unsmob_grob (smob);
  assert (a == Y_AXIS);

  SCM mol = me->get_uncached_stencil ();
  Interval ext;
  if (Stencil *m = unsmob_stencil (mol))
    ext = m->extent (a);
  return ly_interval2scm (ext);
}

/*
  Ugh should have dash-length + dash-period
*/
MAKE_SCHEME_CALLBACK (New_slur, print,1);
SCM
New_slur::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (!scm_ilength (me->get_property ("note-columns")))
    {
      me->suicide ();
      return SCM_EOL;
    }

  Real base_thick = robust_scm2double (me->get_property ("thickness"), 1);
  Real thick = base_thick * Staff_symbol_referencer::line_thickness (me);

  Real ss = Staff_symbol_referencer::staff_space (me);
  Bezier one = get_curve (me);

  Stencil a;

  /*
    TODO: replace dashed with generic property.
  */
  SCM d =  me->get_property ("dashed");
  if (ly_c_number_p (d))
    a = Lookup::dashed_slur (one, thick, thick * robust_scm2double (d, 0));
  else
    a = Lookup::slur (one, get_grob_direction (me) * base_thick * ss / 10.0,
		      thick);

#if DEBUG_SLUR_QUANTING
  SCM quant_score = me->get_property ("quant-score");

  if (to_boolean (me->get_paper ()
		  ->lookup_variable (ly_symbol2scm ("debug-slur-quanting")))
      && ly_c_string_p (quant_score))
    {
      String str;
      SCM properties = Font_interface::text_font_alist_chain (me);

      Stencil tm = *unsmob_stencil (Text_item::interpret_markup
				    (me->get_paper ()->self_scm (), properties,
				     quant_score));
      a.add_at_edge (Y_AXIS, get_grob_direction (me), tm, 1.0, 0);
    }
#endif

  return a.smobbed_copy ();
}


ADD_INTERFACE (New_slur, "new-slur-interface",
	       "A slur",
	       "control-points dashed details direction height-limit note-columns ratio slope-limit thickness");
