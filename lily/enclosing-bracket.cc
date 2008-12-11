/*
  enclosing-bracket.cc -- implement Enclosing_bracket

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "stencil.hh"
#include "horizontal-bracket.hh"
#include "grob.hh"
#include "axis-group-interface.hh"
#include "pointer-group-interface.hh"

struct Enclosing_bracket
{
  DECLARE_GROB_INTERFACE ();
  
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
};


ADD_INTERFACE (Enclosing_bracket,
	      "Brackets alongside bass figures.",
	      
	      /* properties */
	      "bracket-flare "
	      "edge-height "
	      "elements "
	      "padding "
	      "shorten-pair "
	      "thickness "
	      );

/* ugh: should make bracket interface. */


MAKE_SCHEME_CALLBACK (Enclosing_bracket, width, 1);
SCM
Enclosing_bracket::width (SCM grob)
{
  /*
     UGH. cut & paste code.
  */
  Grob *me = unsmob_grob (grob);
  extract_grob_set (me, "elements", elements);
  if (elements.empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Grob *common_x = common_refpoint_of_array (elements, me, X_AXIS); 
  Interval xext = Axis_group_interface::relative_group_extent (elements, common_x, X_AXIS);

  Stencil left_br = Horizontal_bracket::make_bracket (me, 10.0, Y_AXIS, LEFT);
  Stencil right_br = Horizontal_bracket::make_bracket (me, 10.0, Y_AXIS, LEFT);


  xext.widen (robust_scm2double (me->get_property ("padding"), 0.25));
  left_br.translate_axis (xext[LEFT], X_AXIS);
  right_br.translate_axis (xext[RIGHT], X_AXIS);

  left_br.add_stencil (right_br);
  left_br.translate_axis (-me->relative_coordinate (common_x, X_AXIS), X_AXIS);

  return ly_interval2scm (left_br.extent (X_AXIS));
}

MAKE_SCHEME_CALLBACK (Enclosing_bracket, print, 1);
SCM
Enclosing_bracket::print (SCM grob)
{
  Grob *me = unsmob_grob (grob);
  extract_grob_set (me, "elements", elements);
  if (elements.empty ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Grob *common_x = common_refpoint_of_array (elements, me, X_AXIS); 
  Interval xext = Axis_group_interface::relative_group_extent (elements, common_x, X_AXIS);
  if (xext.is_empty ())
    {
      me->programming_error ("elements have no X extent.");
      xext = Interval (0, 0);
    }

  Stencil left_br = Horizontal_bracket::make_enclosing_bracket (me, me, elements,
								Y_AXIS, LEFT);
  Stencil right_br = Horizontal_bracket::make_enclosing_bracket (me, me, elements,
								 Y_AXIS, RIGHT);

  xext.widen (robust_scm2double (me->get_property ("padding"), 0.25));
  left_br.translate_axis (xext[LEFT], X_AXIS);
  right_br.translate_axis (xext[RIGHT], X_AXIS);
  
  left_br.add_stencil (right_br);
  left_br.translate_axis (-me->relative_coordinate (common_x, X_AXIS), X_AXIS);

  return left_br.smobbed_copy ();
}

