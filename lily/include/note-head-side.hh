/*   
  note-head-side.hh -- declare Note_head_side
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef NOTE_HEAD_SIDE_HH
#define NOTE_HEAD_SIDE_HH

#include "item.hh"
#include "directional-element.hh"

/**
   be next to noteheads.

   UGH. another reduplication of Staff_side
   */
class Note_head_side: public virtual Item, public virtual Directional_element
{
public:

  Note_head_side ();
  bool supported_b () const;
  void add_support (Item*);
  VIRTUAL_COPY_CONS (Score_element);
};


#endif /* NOTE_HEAD_SIDE_HH */

