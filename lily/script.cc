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

Script::Script ()
{
  staff_side_l_ =0;
}

void
Script::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  if (o == staff_side_l_)
    staff_side_l_ = dynamic_cast<Staff_side_item*>(n);
}



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
  Graphical_element * e = staff_side_l_->parent_l(X_AXIS);
  translate_axis (e->extent (X_AXIS).center (), X_AXIS);
}

void
Script::do_post_processing ()
{
  Direction d =  staff_side_l_->get_direction ();
  Molecule m (get_molecule(d));

  /*
    UGH UGH UGH
   */
  if (staff_side_l_->get_elt_property ("no-staff-support") == SCM_UNDEFINED) 
    translate_axis (- m.dim_[Y_AXIS][Direction (-d)], Y_AXIS);
}

void
Script::set_staff_side (Staff_side_item*g)
{
  staff_side_l_ = g;
  add_dependency (g);
  set_parent (g, Y_AXIS);
}

Molecule*
Script::do_brew_molecule_p () const
{
  return new Molecule (get_molecule (staff_side_l_->get_direction ()));
}

void
Script::do_print () const
{

}

