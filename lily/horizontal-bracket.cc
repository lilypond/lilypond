/*   
  horizontal-bracket.cc --  implement  Horizontal_bracket

  source file of the GNU LilyPond music typesetter

(c) 2002--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "side-position-interface.hh"
#include "lookup.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"

struct Horizontal_bracket
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static bool has_interface (Grob*);
};


/*
  TODO:

  This doesn't look very elegant: should support winged edges.

  Support texts on the brackets?
*/
MAKE_SCHEME_CALLBACK (Horizontal_bracket, print, 1);

SCM
Horizontal_bracket::print (SCM smob)
{
  Grob * me = unsmob_grob (smob);
  Spanner *sp = dynamic_cast<Spanner*> (me);
  Link_array<Grob> gs = Pointer_group_interface__extract_grobs (me,(Grob*)0, "columns");

  if (!gs.size ())
    {
      me->suicide ();
      return SCM_EOL;
    }
  Grob * cx = common_refpoint_of_array (gs, me, X_AXIS);
  cx = cx->common_refpoint (sp->get_bound (LEFT), X_AXIS);
  cx = cx->common_refpoint (sp->get_bound (RIGHT),X_AXIS);

  Interval ext = gs.top ()->extent (cx, X_AXIS);
  ext.unite (gs[0]->extent (cx, X_AXIS));

  Direction d = get_grob_direction (me);

  Real thickness = Staff_symbol_referencer::line_thickness (me);
  thickness *= robust_scm2double (me->get_property ("thickness"), 1.0);
  
  Stencil b = Lookup::bracket (X_AXIS, ext, thickness, - d* 1.0, thickness/2); 
  
  b.translate_axis ( - sp->get_bound (LEFT)->relative_coordinate (cx, X_AXIS), X_AXIS);

  return b.smobbed_copy ();  
}

ADD_INTERFACE (Horizontal_bracket,"horizontal-bracket-interface",
  "A horizontal bracket encompassing notes.",
  "thickness columns direction");

