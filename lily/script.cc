/*   
  script.cc --  implement Script_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "warn.hh"
#include "script.hh"
#include "font-interface.hh"
#include "side-position-interface.hh"
#include "paper-def.hh"
#include "item.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "stem.hh"
#include "note-column.hh"

Molecule
Script_interface::get_molecule (Grob * me, Direction d)
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

MAKE_SCHEME_CALLBACK (Script_interface,before_line_breaking,1);
SCM
Script_interface::before_line_breaking (SCM smob)
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

  if (Grob * par = me->get_parent (X_AXIS))
    {
      Grob * stem = Note_column::stem_l (par);
      if (stem && Stem::first_head (stem))
	{
	  me->set_parent (Stem::first_head (stem), X_AXIS);
	}
    }
  
  return SCM_UNSPECIFIED;
}


MAKE_SCHEME_CALLBACK (Script_interface,brew_molecule,1);

SCM
Script_interface::brew_molecule (SCM smob)
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



struct Text_script
{
    static bool has_interface (Grob*);
};

struct Skript
{
    static bool has_interface (Grob*);
};

ADD_INTERFACE (Text_script,"text-script-interface",
  "Any text script",
  "script-priority");

ADD_INTERFACE (Script_interface, "script-interface",
  "",
  "script-priority script-molecule staff-support");

