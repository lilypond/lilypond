/*
  mensural-ligature.cc -- implement Mensural_ligature
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002--2004 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "mensural-ligature.hh"

#include <math.h>

#include "item.hh"
#include "font-interface.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "warn.hh"

/*
 * TODO: divide this function into mensural and neo-mensural style.
 *
 * TODO: move this function to class Lookup?
 */
Stencil
brew_flexa (Grob *me,
	    Real interval,
	    bool solid,
	    Real width,
	    Real thickness,
	    bool add_cauda,
	    Direction cauda_direction)
{
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real height = 0.6 * staff_space;
  Stencil stencil;

  if (add_cauda)
    {
      bool consider_interval =
	cauda_direction * interval > 0.0;

      Interval cauda_box_x (0, thickness);
      Interval cauda_box_y;

      if (consider_interval)
        {
	  Real y_length = max (interval/2.0*staff_space, 1.2*staff_space);
	  cauda_box_y = Interval (0, y_length);
	}
      else
	cauda_box_y = Interval (0, staff_space);

      Real y_correction =
	(cauda_direction == UP) ?
	+0.5*height :
	-0.5*height - cauda_box_y.length ();

      Box cauda_box (cauda_box_x, cauda_box_y);
      Stencil cauda = Lookup::filled_box (cauda_box);
      cauda.translate_axis (y_correction, Y_AXIS);
      stencil.add_stencil (cauda);
    }

  Real slope = (interval / 2.0 * staff_space) / width;

  // Compensate optical illusion regarding vertical position of left
  // and right endings due to slope.
  Real ypos_correction = -0.1*staff_space * sign (slope);
  Real slope_correction = 0.2*staff_space * sign (slope);
  Real corrected_slope = slope + slope_correction/width;

  if (solid)
    {
      Stencil solid_head =
	Lookup::beam (corrected_slope, width, height, 0.0);
      stencil.add_stencil (solid_head);
    }
  else // outline
    {
      Stencil left_edge =
	Lookup::beam (corrected_slope, thickness, height, 0.0);
      stencil.add_stencil (left_edge);

      Stencil right_edge =
	Lookup::beam (corrected_slope, thickness, height, 0.0);
      right_edge.translate_axis (width-thickness, X_AXIS);
      right_edge.translate_axis (corrected_slope * (width-thickness), Y_AXIS);
      stencil.add_stencil (right_edge);

      Stencil bottom_edge =
	Lookup::beam (corrected_slope, width, thickness, 0.0);
      bottom_edge.translate_axis (-0.5*height, Y_AXIS);
      stencil.add_stencil (bottom_edge);

      Stencil top_edge =
	Lookup::beam (corrected_slope, width, thickness, 0.0);
      top_edge.translate_axis (+0.5*height, Y_AXIS);
      stencil.add_stencil (top_edge);
    }
  stencil.translate_axis (ypos_correction, Y_AXIS);
  return stencil;
}

Stencil
internal_brew_primitive (Grob *me)
{
  SCM primitive_scm = me->get_property ("primitive");
  if (primitive_scm == SCM_EOL)
    {
      programming_error ("Mensural_ligature:"
			 "undefined primitive -> ignoring grob");
      return Stencil ();
    }

  Stencil out;
  int primitive = scm_to_int (primitive_scm);
  int delta_pitch = 0;
  Real thickness = 0.0;
  Real flexa_width = 0.0;
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  if (primitive & MLP_ANY)
    {
      thickness = robust_scm2double ( me->get_property ("thickness"), .14);
    }

  if (primitive & MLP_FLEXA)
    {
      delta_pitch = robust_scm2int (me->get_property ("delta-pitch"),
				    0);

      flexa_width = robust_scm2double (me->get_property ("flexa-width"), 2.0 * staff_space);
    }

  switch (primitive)
    {
      case MLP_NONE:
	return Stencil ();
      case MLP_BB:
	out = brew_flexa (me, delta_pitch, false,
			  flexa_width, thickness, true, DOWN);
	break;
      case MLP_sc: // mensural brevis head with right cauda
	out = Font_interface::get_default_font (me)->find_by_name ("noteheads.-2mensural");
	break;
      case MLP_ss: // mensural brevis head
	out = Font_interface::get_default_font (me)->find_by_name ("noteheads.-1mensural");
	break;
      case MLP_cs: // mensural brevis head with left cauda
	out = Font_interface::get_default_font (me)->find_by_name ("noteheads.lmensural");
	break;
      case MLP_SS:
	out = brew_flexa (me, delta_pitch, false,
			  flexa_width, thickness, true, UP);
	break;
      case MLP_LB:
	out = brew_flexa (me, delta_pitch, false,
			  flexa_width, thickness, false, CENTER);
	break;
      default:
	programming_error (_f ("Mensural_ligature:"
			       "unexpected case fall-through"));
	return Stencil ();
    }

  SCM join_left_scm = me->get_property ("join-left-amount");
  if (join_left_scm != SCM_EOL)
    {
      int join_left = scm_to_int (join_left_scm);
      if (!join_left)
	programming_error (_f ("Mensural_ligature: (join_left == 0)"));
      Real blotdiameter = (me->get_layout ()->get_dimension (ly_symbol2scm ("blotdiameter")));
      Interval x_extent = Interval (0, thickness);
      Interval y_extent = (join_left > 0) ?
	Interval (-join_left * 0.5 * staff_space, 0) :
	Interval (0, -join_left * 0.5 * staff_space);
      Box join_box (x_extent, y_extent);

      Stencil join = Lookup::round_filled_box (join_box, blotdiameter);
      out.add_stencil (join);
    }

  int pos = Staff_symbol_referencer::get_rounded_position (me);
  if (primitive & MLP_FLEXA)
    {
      pos += delta_pitch;
    }

  return out;
}

MAKE_SCHEME_CALLBACK (Mensural_ligature, brew_ligature_primitive, 1);
SCM
Mensural_ligature::brew_ligature_primitive (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return internal_brew_primitive (me).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Mensural_ligature, print, 1);
SCM
Mensural_ligature::print (SCM)
{
  return SCM_EOL;
}

ADD_INTERFACE (Mensural_ligature, "mensural-ligature-interface",
	       "A mensural ligature",
	       "delta-pitch flexa-width head-width join-left join-left-amount "
	       "ligature-primitive-callback primitive thickness");
