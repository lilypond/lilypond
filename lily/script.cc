/*   
  script.cc --  implement Script
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "debug.hh"
#include "script.hh"
#include "lookup.hh"
#include "side-position-interface.hh"
#include "paper-def.hh"
#include "dimension-cache.hh"

Script ::Script (SCM s)
  : Item (s)
{
}

Molecule
Script::get_molecule(Score_element * me, Direction d)
{
  SCM s = me->get_elt_property ("molecule");
  assert (gh_pair_p (s));

  SCM key = gh_car  (s);
  if (key == ly_symbol2scm ("feta"))
    {
      return me->lookup_l ()->afm_find ("scripts-" +
				    ly_scm2string (index_cell (gh_cdr (s), d)));
    }
  else if (key == ly_symbol2scm ("accordion"))
    {
      return me->lookup_l ()->accordion (gh_cdr (s), me->paper_l()->get_var("interline"));
    }
  else
    assert (false);

  return Molecule ();
}




GLUE_SCORE_ELEMENT(Script,after_line_breaking);
SCM
Script::member_after_line_breaking ()
{
  Side_position_interface i (this);
  Direction d =  i.get_direction ();
  i.set_direction (d);

  return SCM_UNDEFINED;
}


MAKE_SCHEME_SCORE_ELEMENT_CALLBACK(Script,brew_molecule);

SCM
Script::brew_molecule (SCM smob)
{
  Score_element *me= unsmob_element (smob);
  Direction dir = DOWN;
  SCM d = me->get_elt_property ("direction");
  if (isdir_b (d))
    dir = to_dir (d);
  
  return get_molecule (me, dir).create_scheme();
}



