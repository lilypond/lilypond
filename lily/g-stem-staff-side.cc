/*   
  g-stem-staff-side.cc --  implement G_stem_staff_side
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "g-stem-staff-side.hh"
#include "stem.hh"

G_stem_staff_side_item::G_stem_staff_side_item ()
{
  stem_l_ =0;
  relative_dir_ = CENTER;
}

void
G_stem_staff_side_item::do_substitute_element_pointer (Score_element*o,
						       Score_element*n)
{
  if (o == stem_l_)
    {
      stem_l_ = dynamic_cast<Stem*> (n);
    }
}

void
G_stem_staff_side_item::set_stem (Stem*s)
{
  stem_l_ =s;
  add_dependency (s);
}


Direction
G_stem_staff_side_item::get_default_direction () const
{
  return relative_dir_ * stem_l_->dir_;
}

void
G_stem_staff_side_item::do_pre_processing ()
{
  SCM p = remove_elt_property (padding_scm_sym);
  if (p != SCM_BOOL_F && stem_l_)
    {
      p = SCM_CDR (p);
      set_elt_property (padding_scm_sym,
			gh_double2scm(stem_l_->staff_line_leading_f ()
				      * gh_scm2double (p)));
    }
  G_staff_side_item :: do_pre_processing ();
}
