/*   
  note-head-side.hh -- declare Note_head_side
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef NOTE_HEAD_SIDE_HH
#define NOTE_HEAD_SIDE_HH

#include "item.hh"

/**
   be next to noteheads.

   UGH. another reduplication of Staff_side
   */
class Note_head_side: public virtual Item
{
  Link_array<Item> support_l_arr_;
public:
  // naming to avoid conflict with Align_element
  Direction notehead_align_dir_;

  Note_head_side ();
  bool supported_b () const;
  void add_support (Item*);
  VIRTUAL_COPY_CONS (Score_element);
protected:
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual void do_pre_processing();    
};


#endif /* NOTE_HEAD_SIDE_HH */

