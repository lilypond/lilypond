/*   
  system-start-delimiter.cc --  implement System_start_delimiter
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <math.h>

#include "axis-group-interface.hh"
#include "system-start-delimiter.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "all-font-metrics.hh"
#include "score-element.hh"

Molecule
System_start_delimiter::staff_bracket (Score_element*me,Real height)  
{
  Paper_def* p= me->paper_l ();
  SCM scmss = p->get_scmvar ("staffspace");
  Real ss = gh_scm2double (scmss);
  Real arc_height = gh_scm2double (me->get_elt_property("arch-height")) * ss ;
  
  SCM at = gh_list (ly_symbol2scm ("bracket"),
		    scm_product (me->get_elt_property ("arch-angle"), scmss),
		    scm_product (me->get_elt_property ("arch-width"), scmss),
		    gh_double2scm (arc_height),
		    scm_product (me->get_elt_property ("bracket-width"),scmss),
		    gh_double2scm (height),
		    scm_product (me->get_elt_property ("arch-thick"),scmss),
		    scm_product (me->get_elt_property ("bracket-thick"),scmss),
		    SCM_UNDEFINED);

  Real h = height + 2 * arc_height;
  Box b (Interval (0, 1.5 * ss), Interval (-h/2, h/2));
  Molecule mol (b, at);
  mol.align_to (X_AXIS, CENTER);
  return mol;
}

void
System_start_delimiter::set_interface (Score_element*me)
{
  me->set_extent_callback (SCM_EOL, Y_AXIS);
  me->set_interface (ly_symbol2scm ("system-start-delimiter-interface"));
}

bool
System_start_delimiter::has_interface (Score_element*me)
{
  return  me->has_interface (ly_symbol2scm ("system-start-delimiter-interface"));
}

Molecule
System_start_delimiter::simple_bar (Score_element*me,Real h) 
{
  Real w = me->paper_l ()->get_var ("stafflinethickness") *
    gh_scm2double (me->get_elt_property ("thickness"));
  return me->lookup_l ()->filledbox (Box (Interval(0,w), Interval(-h/2, h/2)));
}

MAKE_SCHEME_CALLBACK(System_start_delimiter,after_line_breaking,1);

SCM
System_start_delimiter::after_line_breaking (SCM smob)
{
  try_collapse (unsmob_element (smob));
  return SCM_UNSPECIFIED;
}

void
System_start_delimiter::try_collapse (Score_element*me)
{
  SCM   gl = me->get_elt_property ("glyph");
  
  if (scm_ilength (me->get_elt_property ("elements")) <=  1 && gl == ly_symbol2scm ("bar-line"))
    {
      me->suicide ();
    }
  
}


MAKE_SCHEME_CALLBACK(System_start_delimiter,brew_molecule,1);

SCM
System_start_delimiter::brew_molecule (SCM smob)
{
  Score_element * me = unsmob_element (smob);
  Interval ext = ly_scm2interval (Axis_group_interface::group_extent_callback (me->self_scm(), gh_int2scm (Y_AXIS)));
  Real l = ext.length (); 
  Molecule m;

  SCM s = me->get_elt_property ("collapse-height");
  if (gh_number_p (s) && l < gh_scm2double (s))
    {
      me->suicide();
      return SCM_EOL;
    }

  s = me->get_elt_property ("glyph");
  if (!gh_symbol_p(s))
    return SCM_EOL;
  
  if (s == ly_symbol2scm ("bracket"))
    m = staff_bracket (me,l);
  else if (s == ly_symbol2scm ("brace"))
    m =  staff_brace (me,l);
  else if (s == ly_symbol2scm ("bar-line"))
    m = simple_bar (me,l);
  
  
  m.translate_axis (ext.center (), Y_AXIS);
  return m.create_scheme ();
}

/*
  Ugh. Suck me plenty.
 */
Molecule
System_start_delimiter::staff_brace (Score_element*me,Real y)  
{
  Real staffht  = me->paper_l ()->get_var ("staffheight");
  int staff_size  = int (rint (staffht ));

  // URG
  Real step  = 1.0;
  int minht  = 2 * staff_size;
  int maxht = 7 *  minht;
  int idx = int (((maxht - step) <? y - minht) / step);
  idx = idx >? 0;

  SCM l = scm_assoc (ly_str02scm ("brace"),
		     scm_eval2 (ly_symbol2scm ("cmr-alist"), SCM_EOL));
  
  String nm = "feta-braces";
  if (l != SCM_BOOL_F)
    nm = ly_scm2string (gh_cdr (l));
  nm += to_str (staff_size);
  SCM e =gh_list (ly_symbol2scm ("char"), gh_int2scm (idx), SCM_UNDEFINED);
  SCM at = (e);

  at = fontify_atom (find_font (nm), at);
  
  Box b (Interval (0,0), Interval (-y/2, y/2));

  return Molecule(b, at);
}
  
