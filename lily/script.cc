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
Script::get_molecule(Direction d) const
{
  SCM s = get_elt_property ("molecule");
  assert (gh_pair_p (s));

  SCM key = gh_car  (s);
  if (key == ly_symbol2scm ("feta"))
    {
      return lookup_l ()->afm_find ("scripts-" +
				    ly_scm2string (index_cell (gh_cdr (s), d)));
    }
  else if (key == ly_symbol2scm ("accordion"))
    {
      return lookup_l ()->accordion (gh_cdr (s), paper_l()->get_var("interline"));
    }

  else assert (false);

  return Molecule ();
}


void
Script::before_line_breaking ()
{
  /*
    center my self on the note head.
   */
  Score_element * e = parent_l(X_AXIS);
  translate_axis (e->extent (X_AXIS).center (), X_AXIS);
}

void
Script::after_line_breaking ()
{
  Side_position_interface i (this);
  Direction d =  i.get_direction ();
  i.set_direction (d);
}

Molecule 
Script::do_brew_molecule () const
{
  Direction dir = DOWN;
  SCM d = get_elt_property ("direction");
  if (isdir_b (d))
    dir = to_dir (d);
  
  return get_molecule (dir);
}



