/*   
  staff-side.hh -- declare Staff_side_{element,spanner,item}
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef STAFF_SIDE_HH
#define STAFF_SIDE_HH


#include "spanner.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"

/**
   Position myself next to a set of elements.  Configurable in axis
   and direction.

  Properties:

    padding :: Real

    Amount of extra space to add.
*/
class Staff_side_element :  public Staff_symbol_referencer
{
  void position_self ();
  Direction dir_;
public:
  Score_element * to_position_l_;
  
  void set_direction (Direction d ) { dir_ =  d; }
  Direction get_direction () const { return dir_; }
  

  Link_array<Score_element> support_l_arr_;
  Axis axis_;
  //junkme.
  bool staff_support_b_;
  
  Staff_side_element ();
  void set_victim (Score_element*);
  void add_support (Score_element*);

  VIRTUAL_COPY_CONS(Score_element);
  virtual Direction get_default_direction () const;
protected:
  virtual Interval do_height () const;
  virtual void do_print () const;
  virtual void do_add_processing ();
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual void do_pre_processing ();
  virtual void do_post_processing ();
};

class Staff_side_item : public Staff_side_element, public Item
{
public:
  VIRTUAL_COPY_CONS(Score_element);
protected:
  virtual Interval do_width () const;
  virtual void do_print () const;
};

class Staff_side_spanner : public Staff_side_element, public Spanner
{
public:
  VIRTUAL_COPY_CONS(Score_element);
protected:
  virtual void do_print () const;
};

#endif /* STAFF_SIDE_HH */

