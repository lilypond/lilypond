/*   
  hara-kiri-engraver.cc --  implement Hara_kiri_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "hara-kiri-group-spanner.hh"
#include "hara-kiri-engraver.hh"
#include "rhythmic-head.hh"

Spanner*
Hara_kiri_engraver::get_spanner_p () const
{
  return new Hara_kiri_group_spanner (get_property ("basicHaraKiriVerticalGroupspannerProperties"));
}

void
Hara_kiri_engraver::acknowledge_element (Score_element_info i)
{
  Axis_group_engraver::acknowledge_element (i);

  i.elem_l_->add_offset_callback (Hara_kiri_group_spanner::force_hara_kiri_callback, Y_AXIS);
  
  if (Rhythmic_head *h = dynamic_cast<Rhythmic_head *> (i.elem_l_))
    {
      dynamic_cast<Hara_kiri_group_spanner*> (staffline_p_)
	->add_interesting_item (h);
    }
  
}
ADD_THIS_TRANSLATOR(Hara_kiri_engraver);
