/*   
horizontal-bracket.cc --  implement 

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "side-position-interface.hh"
#include "lookup.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "paper-def.hh"

struct Horizontal_bracket
{
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  static bool has_interface (Grob*);
};


/*
  TODO:

  This doesn't look very elegant: should support winged edges.

  Support texts on the brackets?

*/

MAKE_SCHEME_CALLBACK(Horizontal_bracket, brew_molecule, 1);

SCM
Horizontal_bracket::brew_molecule (SCM smob)
{
  Grob * me = unsmob_grob (smob);
  Spanner *sp = dynamic_cast<Spanner*> (me);
  Link_array<Grob> gs = Pointer_group_interface__extract_grobs (me,(Grob*)0, "columns");

  if (!gs.size())
    {
      me->suicide();
      return SCM_EOL;
    }
  Grob * cx = common_refpoint_of_array (gs, me, X_AXIS);
  cx = cx->common_refpoint (sp->get_bound (LEFT), X_AXIS);
  cx = cx->common_refpoint (sp->get_bound (RIGHT),X_AXIS);

  Interval ext = gs.top()->extent (cx, X_AXIS);
  ext.unite (gs[0]->extent (cx, X_AXIS));

  Direction d = Directional_element_interface::get (me);
  Real t = me->get_paper()->get_var ("linethickness");

  SCM lthick = me->get_grob_property ("thickness");
  if (gh_number_p (lthick))
    t *= gh_scm2double (lthick);
  
  Molecule b = Lookup::bracket (X_AXIS, ext, t, - d* 1.0); 
  
  b.translate_axis ( - sp->get_bound (LEFT)->relative_coordinate (cx, X_AXIS), X_AXIS);

  return b.smobbed_copy();  
}

ADD_INTERFACE (Horizontal_bracket,"horizontal-bracket-interface",
  "A horizontal bracket encompassing notes.",
  "thickness columns direction");

