/*
  line-spanner.cc -- implement Line_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "line-spanner.hh"


#include "spanner.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"
#include "font-interface.hh"
#include "warn.hh"
#include "align-interface.hh"
#include "lookup.hh"
#include "line-interface.hh"

Stencil
zigzag_stencil (Grob *me,
		Offset from,
		Offset to)
{
  Offset dz = to -from;

  Real thick = Staff_symbol_referencer::line_thickness (me);
  thick *= robust_scm2double (me->get_property ("thickness"), 1.0); // todo: staff sym referencer? 

  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Real w = robust_scm2double (me->get_property ("zigzag-width"), 1) * staff_space;
  int count = (int) ceil (dz.length () / w);
  w = dz.length () / count;

  Real l = robust_scm2double (me->get_property ("zigzag-length"), 1) * w;
  Real h = l > w / 2 ? sqrt (l * l - w * w / 4) : 0;

  Offset rotation_factor = complex_exp (Offset (0, dz.arg ()));

  Offset points[3];
  points[0] = Offset (0, -h / 2);
  points[1] = Offset (w / 2, h / 2);
  points[2] = Offset (w, -h / 2);
  for (int i = 0; i < 3; i++)
    points[i] = complex_multiply (points[i], rotation_factor);

  Stencil squiggle (Line_interface::make_line (thick, points[0], points[1]));
  squiggle.add_stencil (Line_interface::make_line (thick, points[1], points[2]));

  Stencil total;
  for (int i = 0; i < count; i++)
    {
      Stencil moved_squiggle (squiggle);
      moved_squiggle.translate (from + Offset (i * w, 0) * rotation_factor);
      total.add_stencil (moved_squiggle);
    }

  Box b;
  b.add_point (Offset (0, 0));
  b.add_point (dz);
  b[X_AXIS].widen (thick / 2);
  b[Y_AXIS].widen (thick / 2);

  return Stencil (b, total.expr ());
}

MAKE_SCHEME_CALLBACK (Line_spanner, after_line_breaking, 1);
SCM
Line_spanner::after_line_breaking (SCM g)
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
Line_spanner::line_stencil (Grob *me,
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
      line = (type == ly_symbol2scm ("zigzag"))
	? zigzag_stencil (me, from, to)
	: Line_interface::line (me, from, to);
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
	mol.add_at_edge (X_AXIS, RIGHT, m, 0, 0);
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

/*
  Find a common Y parent, which --if found-- should be the
  fixed-distance alignment.
*/
Grob *
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

/*
  Warning: this thing is a cross-staff object, so it should have empty Y-dimensions.

  (If not, you risk that this is called from the staff-alignment
  routine, via stencil_extent. At this point, the staves aren't
  separated yet, so it doesn't work cross-staff.

  (huh? crossable staves have fixed distance? --hwn)
*/

MAKE_SCHEME_CALLBACK (Line_spanner, print, 1);
SCM
Line_spanner::print (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));

  Drul_array<Item *> bound (me->get_bound (LEFT),
			    me->get_bound (RIGHT));

  Real gap = robust_scm2double (me->get_property ("gap"), 0.0);

  Offset ofxy (gap, 0); /* offset from start point to start of line */
  Offset dxy;
  Offset my_off;
  Offset his_off;

  if (bound[RIGHT]->break_status_dir ())
    {
      if (bound[LEFT]->break_status_dir ())
	{
	  programming_error ("line-spanner with two broken ends. Farewell sweet world.");

	  me->suicide ();
	  return SCM_EOL;
	}

      /*
	This is hairy. For the normal case, we simply find common
	parents, and draw a line between the bounds. When two note
	heads are on different systems, there is no common parent
	anymore. We have to find the piano-staff object.
      */

      Spanner *next_sp = me->broken_neighbor (RIGHT);
      Item *next_bound = next_sp->get_bound (RIGHT);

      if (next_bound->break_status_dir ())
	{
	  programming_error ("no note heads for the line spanner on next line?"
			     " Confused.");
	  me->suicide ();
	  return SCM_EOL;
	}

      Grob *commonx = bound[LEFT]->common_refpoint (bound[RIGHT], X_AXIS);
      commonx = me->common_refpoint (commonx, X_AXIS);

      Grob *next_common_y = line_spanner_common_parent (next_bound);
      Grob *this_common_y = line_spanner_common_parent (bound[LEFT]);

      Grob *all_common_y = me->common_refpoint (this_common_y, Y_AXIS);

      Interval next_ext = next_bound->extent (next_common_y, Y_AXIS);
      Interval this_ext = bound[LEFT]->extent (this_common_y, Y_AXIS);

      Real yoff = this_common_y->relative_coordinate (all_common_y, Y_AXIS);

      Offset p1 (bound[LEFT]->extent (commonx, X_AXIS)[RIGHT],
		 this_ext.center () + yoff);
      Offset p2 (bound[RIGHT]->extent (commonx, X_AXIS)[LEFT],
		 next_ext.center () + yoff);

      Offset dz (p2 -p1);
      Real len = dz.length ();

      Offset dir = dz * (1 / len);
      dz = (dz.length () - 2 * gap) * dir;

      Stencil l (line_stencil (me, Offset (0, 0), dz));

      l.translate (dir * gap + p1
		   - Offset (me->relative_coordinate (commonx, X_AXIS),
			     me->relative_coordinate (all_common_y, Y_AXIS)));

      return l.smobbed_copy ();
    }
  else
    {
      Grob *common[] = { me, me };
      for (int a = X_AXIS; a < NO_AXES; a++)
	{
	  common[a] = me->common_refpoint (bound[RIGHT], Axis (a));
	  common[a] = common[a]->common_refpoint (bound[LEFT], Axis (a));
	}

      // distance from center to start of line      
      Real off = gap + ((bound[LEFT]->extent (bound[LEFT], X_AXIS).length () * 3) / 4);

      for (int a = X_AXIS; a < NO_AXES; a++)
	{
	  Axis ax = (Axis)a;
	  dxy[ax]
	    = + robust_relative_extent (bound[RIGHT], common[X_AXIS], ax).center ()
	    - robust_relative_extent (bound[LEFT], common[X_AXIS], ax).center ();

	  my_off[ax] = me->relative_coordinate (common[a], ax);
	  his_off[ax] = bound[LEFT]->relative_coordinate (common[a], ax);
	}

      ofxy = dxy * (off / dxy.length ());
      dxy -= 2*ofxy;

      Stencil line = line_stencil (me, Offset (0, 0), dxy);

      line.translate_axis (bound[LEFT]->extent (bound[LEFT], X_AXIS).length () / 2, X_AXIS);
      line.translate (ofxy - my_off + his_off);
      return line.smobbed_copy ();
    }
}

ADD_INTERFACE (Line_spanner, "line-spanner-interface",
	       "Generic line drawn between two objects, e.g. for use with glissandi.\n"
	       "The property @code{style} can be @code{line}, "
	       "@code{dashed-line}, @code{trill}, \n"
	       "@code{dotted-line} or @code{zigzag}.\n"
	       "\n",
	       
	       "arrow "
	       "gap "
	       "thickness "
	       "zigzag-length "
	       "zigzag-width "
	       );

