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
  Real arc_height = p->get_var("bracket_arch_height");
  SCM at = gh_list (ly_symbol2scm ("bracket"),
		    gh_double2scm (p->get_var("bracket_arch_angle")),
		    gh_double2scm (p->get_var("bracket_arch_width")),
		    gh_double2scm (arc_height),
		    gh_double2scm (p->get_var("bracket_width")),
		    gh_double2scm (height),
		    gh_double2scm (p->get_var("bracket_arch_thick")),
		    gh_double2scm (p->get_var("bracket_thick")),
		    SCM_UNDEFINED);

  Real staff_space = p->get_var ("interline");
  Real h = height + 2 * arc_height;
  Box b (Interval (0, 1.5 * staff_space), Interval (-h/2, h/2));
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
  Real w = paper_l ()->get_var ("barthick_score");
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

MAKE_SCHEME_SCORE_ELEMENT_CALLBACKS(System_start_delimiter);
Molecule
System_start_delimiter::do_brew_molecule ()const
{
  Interval ext = Axis_group_interface::group_extent_callback (this, Y_AXIS);
  Real l = ext.length (); 
  Molecule m;

  SCM s = get_elt_property ("collapse-height");
  if (gh_number_p (s) && l < gh_scm2double (s))
    {
      System_start_delimiter * me = (System_start_delimiter*)this;
      me->suicide ();
      return m;
    }

  
  s = get_elt_property ("glyph");
  if (!gh_symbol_p(s))
    return m;
  
  if (s == ly_symbol2scm ("bracket"))
    m = staff_bracket (l);
  else if ( s == ly_symbol2scm ("brace"))
    m =  staff_brace (l);
  else if (s == ly_symbol2scm ("bar-line"))
    m = simple_bar (l);
  
  
  m.translate_axis (ext.center (), Y_AXIS);
  return m;
}

/*
  ugh. Suck me plenty.
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

  SCM l = scm_eval (gh_list (ly_symbol2scm ("style-to-cmr"),
			    ly_str02scm ("brace"),
			    SCM_UNDEFINED));
  
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
  
