/*
  line-spanner.cc -- implement Line_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "font-interface.hh"
#include "grob-interface.hh"
#include "item.hh"
#include "lily-proto.hh"
#include "line-interface.hh"
#include "moment.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "system.hh"
#include "text-interface.hh"
#include "warn.hh"

class Line_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info_and_text, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_right_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_bound_info, (SCM, Direction));
  DECLARE_GROB_INTERFACE ();
};

Spanner *parent_spanner (Grob *g)
{
  if (Spanner::has_interface (g))
    return dynamic_cast<Spanner*> (g);
  return parent_spanner (g->get_parent (Y_AXIS));
}

SCM
Line_spanner::calc_bound_info (SCM smob, Direction dir)
{
  Spanner *me = unsmob_spanner (smob);

  Grob *commonx = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  commonx = me->common_refpoint (commonx, X_AXIS);

  SCM bound_details = me->get_property ("bound-details");

  SCM details = SCM_BOOL_F;
  if (details == SCM_BOOL_F)
    details = ly_assoc_get ((dir == LEFT)
			    ? ly_symbol2scm ("left")
			    : ly_symbol2scm ("right"), bound_details, SCM_BOOL_F);

  if (me->get_bound (dir)->break_status_dir ())
    {
      SCM extra = ly_assoc_get ((dir == LEFT)
				? ly_symbol2scm ("left-broken")
				: ly_symbol2scm ("right-broken"), bound_details, SCM_EOL);

      for (SCM s = scm_reverse (extra); scm_is_pair (s); s = scm_cdr (s))
	details = scm_cons (scm_car (s), details);
    }
  
  if (details == SCM_BOOL_F)
    details = ly_assoc_get (ly_symbol2scm ("default"), bound_details, SCM_EOL);

  SCM text = ly_assoc_get (ly_symbol2scm ("text"), details, SCM_BOOL_F);
  if (Text_interface::is_markup (text))
    {
      Output_def *layout = me->layout ();
      SCM properties = Font_interface::text_font_alist_chain (me);
      details = scm_acons (ly_symbol2scm ("stencil"),
			   Text_interface::interpret_markup (layout->self_scm (),
							     properties, text),
			   details);
    }
  
  if (!scm_is_number (ly_assoc_get (ly_symbol2scm ("X"), details, SCM_BOOL_F)))
    {
      Direction attach = (Direction)
	robust_scm2int (ly_assoc_get (ly_symbol2scm ("attach-dir"),
						     details, SCM_BOOL_F),
			CENTER);

      Item *bound_item = me->get_bound (dir);
      Grob *bound_grob = bound_item;
      if (to_boolean (ly_assoc_get (ly_symbol2scm ("end-on-note"), details, SCM_BOOL_F))
	  && bound_item->break_status_dir ())
	{
	  extract_grob_set (me, "note-columns", columns);
	  if (columns.size ())
	    bound_grob = (dir == LEFT)
	      ? columns[0] : columns.back();
	}
      
      details = scm_acons (ly_symbol2scm ("X"),
			   scm_from_double (robust_relative_extent (bound_grob, commonx, X_AXIS)
					    .linear_combination (attach)),
			   details);
    }
  

  if (!scm_is_number (ly_assoc_get (ly_symbol2scm ("Y"), details, SCM_BOOL_F)))
    {
      Real y = 0.0;

      Real extra_dy = robust_scm2double (me->get_property ("extra-dy"),
					 0.0);

      Grob *common_y = me->common_refpoint (me->get_bound (dir), Y_AXIS);
      if (me->get_bound (dir)->break_status_dir ())
	{
	  Spanner *next_sp = me->broken_neighbor (dir);
	  Item *next_bound = next_sp->get_bound (dir);

	  if (next_bound->break_status_dir ())
	    {
	      programming_error ("no note heads for the line spanner on neighbor line?"
				 " Confused.");
	      me->suicide ();
	      return SCM_EOL;
	    }

	  Spanner *next_bound_parent = parent_spanner (next_bound);
	  Interval next_ext = next_bound->extent (next_bound_parent, Y_AXIS);

	  /* We want to know what would be the
	     y-position of the next bound (relative to my y-parent) if it belonged
	     to the same system as this bound. We rely on the fact that
	     the y-parent of the next bound is a spanner (probably the
	     VerticalAxisGroup of a staff) that extends over the break.
	  */
	  Spanner *next_bound_parent_on_this_line =
	    next_bound_parent->broken_neighbor (other_dir (dir));

	  if (next_bound_parent_on_this_line)
	    {
	      Grob *common = me->common_refpoint (next_bound_parent_on_this_line, Y_AXIS);
	      Real bound_offset = next_bound_parent_on_this_line->relative_coordinate (common, Y_AXIS);
	      y = next_ext.center () + bound_offset - me->relative_coordinate (common, Y_AXIS);
	    }
	  else
	    {
	      /* We fall back to assuming that the distance between staves doesn't
		 change over line breaks. */
	      programming_error ("next-bound's parent doesn't extend to this line");
	      Grob *next_system = next_bound->get_system ();
	      Grob *this_system = me->get_system ();
	      y = next_ext.center () + next_bound_parent->relative_coordinate (next_system, Y_AXIS)
		- me->relative_coordinate (this_system, Y_AXIS);
	    }
	}
      else
	{
	  y = me->get_bound (dir)->extent (common_y, Y_AXIS).center ();
	  details = scm_acons (ly_symbol2scm ("common-Y"), common_y->self_scm (), details);
	}

      y += dir * extra_dy / 2; 
      details = scm_acons (ly_symbol2scm ("Y"), scm_from_double (y), details);
    }

  return details;
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_right_bound_info, 1);
SCM
Line_spanner::calc_right_bound_info (SCM smob)
{
  return Line_spanner::calc_bound_info (smob, RIGHT);
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_left_bound_info, 1);
SCM
Line_spanner::calc_left_bound_info (SCM smob)
{
  return Line_spanner::calc_bound_info (smob, LEFT);
}

MAKE_SCHEME_CALLBACK (Line_spanner, calc_left_bound_info_and_text, 1);
SCM
Line_spanner::calc_left_bound_info_and_text (SCM smob)
{
  SCM alist = Line_spanner::calc_bound_info (smob, LEFT);
  Spanner *me = unsmob_spanner (smob);

  SCM text = me->get_property ("text");
  if (Text_interface::is_markup (text)
      && me->get_bound (LEFT)->break_status_dir () == CENTER
      && ly_assoc_get (ly_symbol2scm ("stencil"), alist, SCM_BOOL_F) == SCM_BOOL_F)
    {
      Output_def *layout = me->layout ();
      SCM properties = Font_interface::text_font_alist_chain (me);
      alist = scm_acons (ly_symbol2scm ("stencil"),
			 Text_interface::interpret_markup (layout->self_scm (),
							   properties, text),
			 alist);
    }
  
  return alist;
}

MAKE_SCHEME_CALLBACK (Line_spanner, print, 1);
SCM
Line_spanner::print (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));

  Interval_t<Moment> moments = me->spanned_time ();
  /*
    We remove the line at the start of the line.  For piano voice
    indicators, it makes no sense to have them at the start of the
    line.

    I'm not sure what the official rules for glissandi are, but
    usually the 2nd note of the glissando is "exact", so when playing
    from the start of the line, there is no need to glide.

    From a typographical p.o.v. this makes sense, since the amount of
    space left of a note at the start of a line is very small.

    --hwn.

  */
  if (moments.length () == Moment (0,0))
    return SCM_EOL;
  
  Drul_array<SCM> bounds (me->get_property ("left-bound-info"),
			  me->get_property ("right-bound-info"));

  
  Grob *commonx = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  commonx = me->common_refpoint (commonx, X_AXIS);

  Drul_array<Offset> span_points;

  Direction d =  LEFT;
  do
    {
      Offset z (robust_scm2double (ly_assoc_get (ly_symbol2scm ("X"),
						 bounds[d], SCM_BOOL_F), 0.0),
		robust_scm2double (ly_assoc_get (ly_symbol2scm ("Y"),
						 bounds[d], SCM_BOOL_F), 0.0));
      
      span_points[d] = z;
    }
  while (flip (&d) != LEFT);

  Drul_array<Real> gaps (0, 0);
  Drul_array<bool> arrows (0, 0);
  Drul_array<Real> anchor_align (0, 0);
  Drul_array<Stencil*> stencils (0,0);
  Drul_array<Grob*> common_y (0, 0);
  do
    {
      gaps[d] = robust_scm2double (ly_assoc_get (ly_symbol2scm ("padding"),
						 bounds[d], SCM_BOOL_F), 0.0);
      arrows[d] = to_boolean (ly_assoc_get (ly_symbol2scm ("arrow"),
					    bounds[d], SCM_BOOL_F));
      anchor_align[d] = robust_scm2double (ly_assoc_get (ly_symbol2scm ("anchor-alignment"),
								 bounds[d], SCM_BOOL_F), LEFT);
      stencils[d] = unsmob_stencil (ly_assoc_get (ly_symbol2scm ("stencil"),
						  bounds[d], SCM_BOOL_F));
      common_y[d] = unsmob_grob (ly_assoc_get (ly_symbol2scm ("common-Y"),
					       bounds[d], SCM_BOOL_F));
      if (!common_y[d])
	common_y[d] = me; 
    }
  while (flip (&d) != LEFT);

  Grob *my_common_y = common_y[LEFT]->common_refpoint (common_y[RIGHT], Y_AXIS);
  do
    span_points[d][Y_AXIS] += common_y[d]->relative_coordinate (my_common_y, Y_AXIS);
  while (flip (&d) != LEFT);

  Offset dz = (span_points[RIGHT] - span_points[LEFT]);
  Offset dz_dir = dz.direction ();
  if (gaps[LEFT] + gaps[RIGHT] > dz.length ())
    {
      return SCM_EOL;
    }

  Stencil line;
  do
    {
      span_points[d] += -d * gaps[d] *  dz.direction ();

      if (stencils[d])
	{
	  Interval ext = stencils[d]->extent (X_AXIS);
	  Real anchor = ext.linear_combination (anchor_align[d]) - ext[LEFT];
	  span_points[d][X_AXIS] -= anchor;

	  Stencil s = stencils[d]->translated (span_points[d]);
	  SCM align = ly_assoc_get (ly_symbol2scm ("stencil-align-dir-y"),
				    bounds[d], SCM_BOOL_F);
	  SCM off = ly_assoc_get (ly_symbol2scm ("stencil-offset"),
				  bounds[d], SCM_BOOL_F);

	  if (scm_is_number (align)) 
	    s.align_to (Y_AXIS, scm_to_double (align));

	  /*
	    todo: should use font-size.
	  */
	  if (is_number_pair (off))
	    s.translate (ly_scm2offset (off));
	 
	  line.add_stencil (s);
	}
    }
  while (flip (&d) != LEFT);

  do
    {
      if (stencils[d])
	span_points[d] += dz_dir *
	  (stencils[d]->extent (X_AXIS)[-d] / dz_dir[X_AXIS]);
    }
  while (flip (&d) != LEFT);

  Offset adjust = dz.direction() * Staff_symbol_referencer::staff_space (me);

  Offset line_left = span_points[LEFT] + (arrows[LEFT] ? adjust*1.4 : Offset (0, 0));
  Offset line_right = span_points[RIGHT] - (arrows[RIGHT] ? adjust*0.55 : Offset (0, 0));
  if (line_right[X_AXIS] > line_left[X_AXIS])
    {
      line.add_stencil (Line_interface::line (me, line_left, line_right));
 
      line.add_stencil (Line_interface::arrows (me,
						span_points[LEFT],
						span_points[RIGHT],
						arrows[LEFT],
						arrows[RIGHT]));
    }

  line.translate (Offset (-me->relative_coordinate (commonx, X_AXIS),
			  -me->relative_coordinate (my_common_y, Y_AXIS)));
			  

  return line.smobbed_copy ();
}

ADD_INTERFACE (Line_spanner,
	       "Generic line drawn between two objects, e.g., for use with"
	       " glissandi.\n"
	       "\n"
	       "The property @code{style} can be @code{line},"
	       " @code{dashed-line}, @code{trill}, @code{dotted-line} or"
	       " @code{zigzag}.",

	       /* properties */
	       "bound-details "
	       "extra-dy "
	       "gap "
	       "left-bound-info "
	       "note-columns "
	       "right-bound-info "
	       "thickness "
	       "to-barline "
	       );

