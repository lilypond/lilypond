/*   
  pitch-squash-grav.cc --  implement Pitch_squash_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
  
 */

#include "pitch-squash-grav.hh"
#include "note-head.hh"

void
Pitch_squash_engraver::acknowledge_element (Score_elem_info i)
{
  if (i.elem_l_->is_type_b (Note_head::static_name ()))
    {
      Note_head * nl = (Note_head*)i.elem_l_->item ();
      nl->position_i_  =0;
    }
}

ADD_THIS_TRANSLATOR (Pitch_squash_engraver);
IMPLEMENT_IS_TYPE_B1(Pitch_squash_engraver, Engraver);

