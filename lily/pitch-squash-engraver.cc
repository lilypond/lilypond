/*   
  pitch-squash-grav.cc --  implement Pitch_squash_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "staff-symbol-referencer.hh"

#include "pitch-squash-engraver.hh"
#include "rhythmic-head.hh"

void
Pitch_squash_engraver::acknowledge_element (Score_element_info i)
{
  if (to_boolean (i.elem_l_->get_elt_property ("note-head-interface")))
    {
      Staff_symbol_referencer_interface (i.elem_l_).set_position(0);
    }
}

ADD_THIS_TRANSLATOR (Pitch_squash_engraver);


