/*   
  pitch-squash-grav.cc --  implement Pitch_squash_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "pitch-squash-engraver.hh"
#include "note-head.hh"

void
Pitch_squash_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_head *nh = dynamic_cast<Note_head *> (i.elem_l_))
    {
      nh->position_i_  =0;
    }
}

ADD_THIS_TRANSLATOR (Pitch_squash_engraver);


