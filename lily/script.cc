/*   
  script.cc --  implement Script
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
Script::get_molecule (Grob * me, Direction d)
{
  SCM s = me->get_grob_property ("script-molecule");
  assert (gh_pair_p (s));

  SCM key = ly_car (s);
  if (key == ly_symbol2scm ("feta"))
    {
      return Font_interface::get_default_font (me)->find_by_name ("scripts-" +
				    ly_scm2string (index_cell (ly_cdr (s), d)));
    }
  else if (key == ly_symbol2scm ("accordion"))
    {
      return Lookup::accordion (ly_cdr (s), 1.0, Font_interface::get_default_font (me));
    }
  else
    assert (false);

  return Molecule ();
}

MAKE_SCHEME_CALLBACK (Script,before_line_breaking,1);
SCM
Script::before_line_breaking (SCM smob)
{
  Grob * me = unsmob_grob (smob);

  Direction d = Side_position_interface::get_direction (me);

  if (!d)
    {
  /*
    we should not have `arbitrary' directions. 
   */
      programming_error ("Script direction not yet known!");
      d = DOWN;
    }
  
  Side_position_interface::set_direction (me,d);

  return SCM_UNSPECIFIED;
}


MAKE_SCHEME_CALLBACK (Script,brew_molecule,1);

SCM
Script::brew_molecule (SCM smob)
{
  Grob *me= unsmob_grob (smob);

  Direction dir = Side_position_interface::get_direction (me);
  if (!dir)
    {
      programming_error ("Script direction not known, but molecule wanted.");
      dir= DOWN;
    }
  
  return get_molecule (me, dir).smobbed_copy ();
}

bool
Script::has_interface (Grob*me)
{
  return me->has_interface (ly_symbol2scm ("script-interface"));
}


ADD_INTERFACE (Text_script,"text-script-interface",
  "Any text script",
  "script-priority");

ADD_INTERFACE (Skript, "script-interface",
  "",
  "script-priority script-molecule staff-support");

