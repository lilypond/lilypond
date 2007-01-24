/*
  line-spanner.cc -- implement New_line_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "spanner.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"
#include "font-interface.hh"
#include "warn.hh"
#include "align-interface.hh"
#include "lookup.hh"
#include "line-interface.hh"
#include "moment.hh"

#include "lily-proto.hh"
#include "grob-interface.hh"

class New_line_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));

  DECLARE_SCHEME_CALLBACK (calc_left_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_right_bound_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_bound_info, (SCM, Direction));
  DECLARE_GROB_INTERFACE();

  static Stencil line_stencil (Grob *me, Offset f, Offset t);
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

MAKE_SCHEME_CALLBACK (New_line_spanner, after_line_breaking, 1);
SCM
New_line_spanner::after_line_breaking (SCM g)
{
  Grob *me = unsmob_grob (g);
  Spanner *sp = dynamic_cast<Spanner *> (me);

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
  if (sp->get_bound (LEFT)->break_status_dir ()
      && !sp->get_bound (RIGHT)->break_status_dir ())
    {
      /*
	Can't do suicide, since this mucks up finding the trend.
      */
      me->set_property ("transparent", SCM_BOOL_T);
    }
  return SCM_EOL;
}

Stencil
New_line_spanner::line_stencil (Grob *me,
			    Offset from,
			    Offset to)
{
  Offset dz = to -from;
  SCM type = me->get_property ("style");

  Stencil line;

  if (scm_is_symbol (type)
      && (type == ly_symbol2scm ("line")
	  || type == ly_symbol2scm ("dashed-line")
	  || type == ly_symbol2scm ("dotted-line")
	  || type == ly_symbol2scm ("zigzag")
	  || (type == ly_symbol2scm ("trill") && dz[Y_AXIS] != 0)))
    {
      line = Line_interface::line (me, from, to);
    }
  else if (scm_is_symbol (type)
	   && type == ly_symbol2scm ("trill"))
    {
      SCM alist_chain = Font_interface::text_font_alist_chain (me);
      SCM style_alist = scm_list_n (scm_cons (ly_symbol2scm ("font-encoding"),
					      ly_symbol2scm ("fetaMusic")),
				    SCM_UNDEFINED);

      Font_metric *fm = select_font (me->layout (),
				     scm_cons (style_alist,
					       alist_chain));
      Stencil m = fm->find_by_name ("scripts.trill_element");
      Stencil mol;

      do
	mol.add_at_edge (X_AXIS, RIGHT, m, 0);
      while (m.extent (X_AXIS).length ()
	     && mol.extent (X_AXIS).length ()
	     + m.extent (X_AXIS).length () < dz[X_AXIS])
	;

      /*
	FIXME: should center element on x/y
      */
      mol.translate_axis (m.extent (X_AXIS).length () / 2, X_AXIS);
      mol.translate_axis (- (mol.extent (Y_AXIS)[DOWN]
			     + mol.extent (Y_AXIS).length ()) / 2, Y_AXIS);

      mol.translate (from);
      line = mol;
    }

  if (to_boolean (me->get_property ("arrow")))
    line.add_stencil (Line_interface::arrows (me, from, to, false, true));

  return line;
}


SCM
New_line_spanner::calc_bound_info (SCM smob, Direction dir)
{
  Spanner *me = unsmob_spanner (smob);

  Grob *commonx = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);
  commonx = me->common_refpoint (commonx, X_AXIS);

  SCM bound_details = me->get_property ("bound-details");

  
  SCM sym = 
    (me->get_bound (dir)->break_status_dir ())
    ? (dir == LEFT ? ly_symbol2scm ("left-broken")
       : ly_symbol2scm ("right-broken"))
    : (dir == LEFT ? ly_symbol2scm ("left")
       : ly_symbol2scm ("right"));

  SCM details = ly_assoc_get (sym, bound_details, SCM_BOOL_F);
  if (details == SCM_BOOL_F)
    details = ly_assoc_get (ly_symbol2scm ("default"), bound_details, SCM_EOL);

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
	  Grob *commony = me->get_bound (LEFT);
	  commony = me->common_refpoint (commony, Y_AXIS);
	  y = me->get_bound (dir)->extent (commony, Y_AXIS).center();
	}

      y += dir * extra_dy / 2; 
      details = scm_acons (ly_symbol2scm ("Y"), scm_from_double (y), details);
    }

  return details;
}

MAKE_SCHEME_CALLBACK (New_line_spanner, calc_right_bound_info, 1);
SCM
New_line_spanner::calc_right_bound_info (SCM smob)
{
  return New_line_spanner::calc_bound_info (smob, RIGHT);
}

MAKE_SCHEME_CALLBACK (New_line_spanner, calc_left_bound_info, 1);
SCM
New_line_spanner::calc_left_bound_info (SCM smob)
{
  return New_line_spanner::calc_bound_info (smob, LEFT);
}

MAKE_SCHEME_CALLBACK (New_line_spanner, print, 1);
SCM
New_line_spanner::print (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));

  Interval_t<Moment> moments = me->spanned_time ();
  if (moments.length () == Moment (0,0))
    {
      me->set_property ("transparent", SCM_BOOL_T);
      return SCM_EOL;
    }
  
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

  Offset dz = (span_points[RIGHT] - span_points[LEFT]);
  Drul_array<Real> gaps;
  do
     gaps[d] = robust_scm2double (ly_assoc_get (ly_symbol2scm ("padding"),
						bounds[d], SCM_BOOL_F), 0.0);
  while (flip (&d) != LEFT);

  if (gaps[LEFT] + gaps[RIGHT] > dz.length ())
    {
      me->set_property ("transparent", SCM_BOOL_T);
      return SCM_EOL;
    }

  do
    span_points[d] += -d * gaps[d] *  dz.direction ();
  while (flip (&d) != LEFT);

  Offset my_z (me->relative_coordinate (commonx, X_AXIS), 0);
  
  span_points[LEFT] -= my_z;
  span_points[RIGHT] -= my_z;
  
  Stencil line = line_stencil (me,
			       span_points[LEFT],
			       span_points[RIGHT]);

  
  return line.smobbed_copy ();
}

ADD_INTERFACE (New_line_spanner,
	       "Generic line drawn between two objects, e.g. for use with glissandi.\n"
	       "The property @code{style} can be @code{line}, "
	       "@code{dashed-line}, @code{trill}, \n"
	       "@code{dotted-line} or @code{zigzag}.\n"
	       "\n",

	       "extra-dy "
	       "arrow "
	       "gap "
	       "thickness "
	       "bound-details " 
	       "left-bound-info " 
	       "right-bound-info " 
	       );

