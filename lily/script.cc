/*   
     script.cc --  implement Script
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

/*

  TODO: Quantisation support (staccato dots between stafflines)

*/
#include "debug.hh"
#include "script.hh"
#include "lookup.hh"
#include "staff-side.hh"
#include "paper-def.hh"
#include "dimension-cache.hh"


Molecule
Script::get_molecule(Direction d) const
{
  SCM s = get_elt_property ("molecule");
  assert (s != SCM_UNDEFINED);

  SCM key = SCM_CAR (s);
  if (key == ly_symbol2scm ("feta"))
    {
      return lookup_l ()->afm_find ("scripts-" +
				    ly_scm2string (index_cell (gh_cdr (s), d)));
    }
  else if (key == ly_symbol2scm ("accordion"))
    {
      return lookup_l ()->accordion (s, paper_l()->get_var("interline"));
    }

  else assert (false);

  return Molecule ();
}


void
Script::do_pre_processing ()
{
  /*
    center my self on the note head.
   */
  Graphical_element * e = parent_l(X_AXIS);
  translate_axis (e->extent (X_AXIS).center (), X_AXIS);
}

void
Script::do_post_processing ()
{
  Direction d =  Staff_sidify (this).get_direction ();
  Molecule m (get_molecule(d));

  /*
    UGH UGH UGH
   */
#if 0
  if (staff_side_l_->get_elt_property ("no-staff-support") == SCM_UNDEFINED) 
    translate_axis (- m.dim_[Y_AXIS][Direction (-d)], Y_AXIS);
#endif
}


Molecule*
Script::do_brew_molecule_p () const
{
  Direction dir = DOWN;
  SCM d = get_elt_property ("direction");
  if (isdir_b (d))
    dir = to_dir (d);
  
  return new Molecule (get_molecule (dir));
}



