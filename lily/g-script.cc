/*   
  g-script.cc --  implement G_script
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

/*

  TODO: Quantisation support (staccato dots between stafflines)

*/
#include "debug.hh"
#include "g-script.hh"
#include "lookup.hh"
#include "g-staff-side.hh"

G_script::G_script ()
{
  staff_side_l_ =0;
}

void
G_script::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  if (o == staff_side_l_)
    staff_side_l_ = dynamic_cast<G_staff_side_item*>(n);
}



Molecule
G_script::get_molecule(Direction d) const
{
  SCM s = get_elt_property (molecule_scm_sym);
  assert  (s != SCM_BOOL_F);

  s = SCM_CDR(s);
  SCM key = SCM_CAR (s);
  if (key == ly_symbol ("feta"))
    {
      return lookup_l ()->afm_find ("scripts-" +
				    ly_scm2string (index_cell (SCM_CDR (s), d)));
    }
  else if (key == ly_symbol ("accordion"))
    {
      return lookup_l ()->accordion (SCM_CDR(s));
    }

  else assert (false);

  return Molecule ();
}


void
G_script::do_pre_processing ()
{
  Graphical_element * e
    = staff_side_l_->dim_cache_[X_AXIS]->parent_l_->element_l();
  translate_axis (e->extent (X_AXIS).center (), X_AXIS);
}

void
G_script::do_post_processing ()
{
  Direction d =  staff_side_l_->dir_;
  Molecule m (get_molecule(d));
  translate_axis (- m.dim_[Y_AXIS][Direction (-d)], Y_AXIS);
}

void
G_script::set_staff_side (G_staff_side_item*g)
{
  staff_side_l_ = g;
  add_dependency (g);
  dim_cache_[Y_AXIS]->parent_l_ = g->dim_cache_[Y_AXIS];
}

Molecule*
G_script::do_brew_molecule_p () const
{
  return new Molecule (get_molecule (staff_side_l_->dir_));
}

void
G_script::do_print () const
{

}
