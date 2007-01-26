/*
  line-spanner.cc -- implement Line_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "spanner.hh"
#include "output-def.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "font-interface.hh"
#include "warn.hh"
#include "align-interface.hh"
#include "line-interface.hh"
#include "moment.hh"

#include "lily-proto.hh"
#include "grob-interface.hh"
#include "text-interface.hh"

class Line_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_left_bound_info_and_text, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_right_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_bound_info, (SCM, Direction));
  DECLARE_GROB_INTERFACE();
};


static Grob *
line_spanner_common_parent (Grob *me)
{
  Grob *common = find_fixed_alignment_parent (me);
  if (!common)
    {
      common = Staff_symbol_referencer::get_staff_symbol (me);
      if (common)
	common = common->get_parent (Y_AXIS);
      else
	common = me->get_parent (Y_AXIS);
    }

  return common;
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

      for (SCM s = extra; scm_is_pair (s); s = scm_cdr (s))
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

      details = scm_acons (ly_symbol2scm ("X"),
			   scm_from_double (me->get_bound (dir)->extent (commonx, X_AXIS)
					    .linear_combination (attach)),
			   details);
    }
  

  if (!scm_is_number (ly_assoc_get (ly_symbol2scm ("Y"), details, SCM_BOOL_F)))
    {
      Real y = 0.0;

      Real extra_dy = robust_scm2double (me->get_property ("extra-dy"),
					 0.0);
         
      if (me->get_bound (dir)->break_status_dir ())
	{
	  /*
	    This is hairy. For the normal case, we simply find common
	    parents, and draw a line between the bounds. When two note
	    heads are on different systems, there is no common parent
	    anymore. We have to find the piano-staff object.
	  */

	  Spanner *next_sp = me->broken_neighbor (dir);
	  Item *next_bound = next_sp->get_bound (dir);

	  if (next_bound->break_status_dir ())
	    {
	      programming_error ("no note heads for the line spanner on neighbor line?"
				 " Confused.");
	      me->suicide ();
	      return SCM_EOL;
	    }

	  Grob *next_common_y = line_spanner_common_parent (next_bound);
	  Interval next_ext = next_bound->extent (next_common_y, Y_AXIS);

	  y = next_ext.center ();
	}
      else
	{
	  Grob *commony = me->common_refpoint (me->get_bound (dir), Y_AXIS);
	  y = me->get_bound (dir)->extent (commony, Y_AXIS).center();
	  details = scm_acons (ly_symbol2scm ("common-Y"), commony->self_scm (), details);
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
						 bounds[d], SCM_BOOL_F), 0.0)
		+ commonx->relative_coordinate (commonx, X_AXIS),
		robust_scm2double (ly_assoc_get (ly_symbol2scm ("Y"),
						 bounds[d], SCM_BOOL_F), 0.0));
      
      span_points[d] = z;
    }
  while (flip (&d) != LEFT);

  Drul_array<Real> gaps (0, 0);
  Drul_array<bool> arrows (0, 0);
  Drul_array<Stencil*> stencils (0,0);
  Drul_array<Grob*> common_y (0, 0);
  do
    {
      gaps[d] = robust_scm2double (ly_assoc_get (ly_symbol2scm ("padding"),
						 bounds[d], SCM_BOOL_F), 0.0);
      arrows[d] = to_boolean (ly_assoc_get (ly_symbol2scm ("arrow"),
					    bounds[d], SCM_BOOL_F));
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
      if (stencils[d])
	{
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
      
      span_points[d] += -d * gaps[d] *  dz.direction ();
    }
  while (flip (&d) != LEFT);

  line.add_stencil (Line_interface::line (me, 
					  span_points[LEFT],
					  span_points[RIGHT]));

  line.add_stencil (Line_interface::arrows (me,
					    span_points[LEFT],
					    span_points[RIGHT],
					    arrows[LEFT],
					    arrows[RIGHT]));

  line.translate (Offset (-me->relative_coordinate (commonx, X_AXIS),
			  -me->relative_coordinate (my_common_y, Y_AXIS)));
			  
    
  return line.smobbed_copy ();
}

ADD_INTERFACE (Line_spanner,
	       "Generic line drawn between two objects, e.g. for use with glissandi.\n"
	       "The property @code{style} can be @code{line}, "
	       "@code{dashed-line}, @code{trill}, \n"
	       "@code{dotted-line} or @code{zigzag}.\n"
	       "\n",

	       "extra-dy "
	       "gap "
	       "thickness "
	       "bound-details " 
	       "left-bound-info " 
	       "right-bound-info " 
	       );

