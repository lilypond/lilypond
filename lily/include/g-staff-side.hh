/*   
  g-staff-side.hh -- declare G_staff_side_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef G_STAFF_SIDE_HH
#define G_STAFF_SIDE_HH

#include "item.hh"
#include "staff-symbol-referencer.hh"

class G_staff_side_item : public Item, public Staff_symbol_referencer
{
  void position_self ();
public:
  Score_element * to_position_l_;
  Direction dir_;
  Link_array<Score_element> support_l_arr_;
  Real padding_f_;
  Axis axis_;
  
  G_staff_side_item ();
  void set_victim (Score_element*);
  void add_support (Score_element*);

  virtual void set_default_direction ();
  VIRTUAL_COPY_CONS(Score_element);
protected:
  virtual void do_add_processing ();
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual void do_pre_processing ();
  virtual void do_post_processing ();
};

#endif /* G_STAFF_SIDE_HH */

