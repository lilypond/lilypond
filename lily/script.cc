/*   
  script.cc --  implement Script
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "debug.hh"
#include "script.hh"
#include "font-interface.hh"
#include "side-position-interface.hh"
#include "paper-def.hh"
#include "item.hh"
#include "molecule.hh"
#include "lookup.hh"

Molecule
Script::get_molecule(Score_element * me, Direction d)
{
  SCM s = me->get_elt_property ("molecule");
  assert (gh_pair_p (s));

  SCM key = gh_car  (s);
  if (key == ly_symbol2scm ("feta"))
    {
      return Font_interface::get_default_font (me)->find_by_name ("scripts-" +
				    ly_scm2string (index_cell (gh_cdr (s), d)));
    }
  else if (key == ly_symbol2scm ("accordion"))
    {
      return Lookup::accordion (gh_cdr (s), 1.0, Font_interface::get_default_font (me));
    }
  else
    assert (false);

  return Molecule ();
}

MAKE_SCHEME_CALLBACK(Script,after_line_breaking,1);
SCM
Script::after_line_breaking (SCM smob)
{
  Score_element * me = unsmob_element (smob);

  Direction d = Side_position::get_direction (me);
  Side_position::set_direction (me,d);

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK(Script,brew_molecule,1);

SCM
Script::brew_molecule (SCM smob)
{
  Score_element *me= unsmob_element (smob);
//   Direction dir = DOWN;
//   SCM d = me->get_elt_property ("direction");
//   if (isdir_b (d))
//     dir = to_dir (d);
  Direction dir = Side_position::get_direction(me);
  return get_molecule (me, dir).create_scheme();
}

bool
Script::has_interface (Score_element*me)
{
  return me->has_interface (ly_symbol2scm ("script-interface"));
}

void
Script::set_interface (Score_element*me)
{
  return me->set_interface (ly_symbol2scm ("script-interface"));
}
