/*   
  pitch-squash-grav.cc --  implement Pitch_squash_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "pitch-squash-engraver.hh"
#include "note-head.hh"

void
Pitch_squash_engraver::acknowledge_element (Score_element_info i)
{
  if (i.elem_l_->is_type_b (Note_head::static_name ()))
    {
      Note_head * nl = (Note_head*)dynamic_cast <Item *> (i.elem_l_);
      nl->position_i_  =0;
    }
}

ADD_THIS_TRANSLATOR (Pitch_squash_engraver);
IMPLEMENT_IS_TYPE_B1(Pitch_squash_engraver, Engraver);

