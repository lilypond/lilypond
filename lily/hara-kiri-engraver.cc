/*   
  hara-kiri-engraver.cc --  implement Hara_kiri_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "hara-kiri-group-spanner.hh"
#include "hara-kiri-engraver.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"

Spanner*
Hara_kiri_engraver::get_spanner_p () const
{
  Spanner * sp = new Spanner (get_property ("basicHaraKiriVerticalGroupspannerProperties"));
  Hara_kiri_group_spanner::set_interface (sp);
  return sp;
}

void
Hara_kiri_engraver::acknowledge_element (Score_element_info i)
{
  Axis_group_engraver::acknowledge_element (i);

  i.elem_l_->add_offset_callback (Hara_kiri_group_spanner::force_hara_kiri_callback, Y_AXIS);
  
  if (Rhythmic_head *h = dynamic_cast<Rhythmic_head *> (i.elem_l_))
    {
      Hara_kiri_group_spanner::add_interesting_item (staffline_p_, h);
    }
}
ADD_THIS_TRANSLATOR(Hara_kiri_engraver);
