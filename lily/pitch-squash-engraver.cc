/*   
  pitch-squash-grav.cc --  implement Pitch_squash_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "staff-symbol-referencer.hh"
#include "note-head.hh"
#include "pitch-squash-engraver.hh"
#include "rhythmic-head.hh"

void
Pitch_squash_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_head::has_interface (i.elem_l_))
    {
      Staff_symbol_referencer::set_position (i.elem_l_,0);
    }
}

ADD_THIS_TRANSLATOR (Pitch_squash_engraver);


