/*   
  g-staff-side.hh -- declare G_staff_side_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef G_STAFF_SIDE_HH
#define G_STAFF_SIDE_HH

#include "item.hh"

/*
  DO NOT USE AS BREAKABLE ITEM!

  (-> core dump!)
 */
class G_staff_side_item : public Item
{
public:
  Score_element * to_position_l_;
  Direction dir_;
  Link_array<Score_element> support_l_arr_;
  
  G_staff_side_item ();
  void set_victim (Score_element*);
  void add_support (Score_element*);

  virtual void set_default_direction ();
  VIRTUAL_COPY_CONS(Score_element);
protected:
  virtual void do_substitute_dependency (Score_element*,Score_element*);
  virtual void do_pre_processing ();
  virtual void do_post_processing ();
};

#endif /* G_STAFF_SIDE_HH */

