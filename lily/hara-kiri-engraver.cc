/*   
  hara-kiri-engraver.cc --  implement Hara_kiri_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "hara-kiri-vertical-group-spanner.hh"
#include "hara-kiri-engraver.hh"
#include "rhythmic-head.hh"

Axis_group_spanner*
Hara_kiri_engraver::get_spanner_p () const
{
  return new Hara_kiri_group_spanner;
}

void
Hara_kiri_engraver::acknowledge_element (Score_element_info i)
{
  Axis_group_engraver::acknowledge_element (i);
  if (Rhythmic_head *h = dynamic_cast<Rhythmic_head *> (i.elem_l_))
    {
      dynamic_cast<Hara_kiri_group_spanner*> (staffline_p_)
	->add_interesting_item (h);
    }
  
}
ADD_THIS_TRANSLATOR(Hara_kiri_engraver);
