/*   
  system-start-delimiter.cc --  implement System_start_delimiter
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <math.h>

#include "system-start-delimiter.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "axis-group-interface.hh"
#include "lookup.hh"
#include "all-font-metrics.hh"

Molecule
System_start_delimiter::staff_bracket (Real height) const 
{
  Paper_def* p= paper_l ();
  SCM scmss = p->get_scmvar ("staffspace");
  Real ss = gh_scm2double (scmss);
  Real arc_height = gh_scm2double (get_elt_property("arch-height")) * ss ;
  
  SCM at = gh_list (ly_symbol2scm ("bracket"),
		    scm_product (get_elt_property ("arch-angle"), scmss),
		    scm_product (get_elt_property ("arch-width"), scmss),
		    gh_double2scm (arc_height),
		    scm_product (get_elt_property ("bracket-width"),scmss),
		    gh_double2scm (height),
		    scm_product (get_elt_property ("arch-thick"),scmss),
		    scm_product (get_elt_property ("bracket-thick"),scmss),
		    SCM_UNDEFINED);

  Real h = height + 2 * arc_height;
  Box b (Interval (0, 1.5 * ss), Interval (-h/2, h/2));
  Molecule mol (b, at);
  mol.align_to (X_AXIS, CENTER);
  return mol;
}

System_start_delimiter::System_start_delimiter (SCM s)
  : Spanner (s)
{
  set_extent_callback (0, Y_AXIS);
  Pointer_group_interface (this).set_interface();
}

Molecule
System_start_delimiter::simple_bar (Real h) const
{
  Real w = paper_l ()->get_var ("stafflinethickness") *
    gh_scm2double (get_elt_property ("thickness"));
  return lookup_l ()->filledbox (Box (Interval(0,w), Interval(-h/2, h/2)));
}

void
System_start_delimiter::after_line_breaking ()
{
  SCM   gl = get_elt_property ("glyph");
  
  if (scm_ilength (get_elt_pointer ("elements")) <=  1 && gl == ly_symbol2scm ("bar-line"))
    {
      suicide ();
    }
}

MAKE_SCHEME_SCORE_ELEMENT_NON_DEFAULT_CALLBACKS(System_start_delimiter);

SCM
System_start_delimiter::scheme_molecule (SCM smob)
{
  Score_element * sc = unsmob_element (smob);

  System_start_delimiter * ssd= dynamic_cast<System_start_delimiter*> (sc);
  
  Interval ext = Axis_group_interface::group_extent_callback (sc, Y_AXIS);
  Real l = ext.length (); 
  Molecule m;

  SCM s = sc->get_elt_property ("collapse-height");
  if (gh_number_p (s) && l < gh_scm2double (s))
    {
      sc->suicide();
      return SCM_EOL;
    }

  s = sc->get_elt_property ("glyph");
  if (!gh_symbol_p(s))
    return SCM_EOL;
  
  if (s == ly_symbol2scm ("bracket"))
    m = ssd->staff_bracket (l);
  else if (s == ly_symbol2scm ("brace"))
    m = ssd-> staff_brace (l);
  else if (s == ly_symbol2scm ("bar-line"))
    m = ssd->simple_bar (l);
  
  
  m.translate_axis (ext.center (), Y_AXIS);
  return m.create_scheme ();
}

/*
  Ugh. Suck me plenty.
 */
Molecule
System_start_delimiter::staff_brace (Real y)  const
{
  Real staffht  = paper_l ()->get_var ("staffheight");
  int staff_size  = int (rint (staffht ));

  // URG
  Real step  = 1.0;
  int minht  = 2 * staff_size;
  int maxht = 7 *  minht;
  int idx = int (((maxht - step) <? y - minht) / step);
  idx = idx >? 0;

  SCM l = scm_assoc (ly_str02scm ("brace"),
		     scm_eval (ly_symbol2scm ("cmr-alist")));
  
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
  
