/*
  align-item.hh -- declare Align_elem

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VERTICAL_ALIGN_ITEM_HH
#define VERTICAL_ALIGN_ITEM_HH

#include "score-element.hh"
#include "interval.hh"
#include "direction.hh"
#include "axes.hh"

/**
  Order elements top to bottom.

  TODO: merge with Horizontal_align_item
 */
class Align_element : virtual public Score_element {
  Link_array<Score_element> elem_l_arr_;
  Array<int> priority_i_arr_;
  void sort_elements ();
public:
  Interval threshold_interval_ ;

  /**
     Should high priorities be first or last?
   */
    
  Direction stacking_dir_;

  /**
     Which side to align? 
     -1: left side, 0: centered (around center_l_ if not nil), 1: right side
  */

  Direction align_dir_;
  
  Axis axis_;
  Score_element * center_l_;
  
  Align_element ();
  void add_element (Score_element*);
  void add_element_priority (Score_element*, int);
  bool contains_b (Score_element const*) const;
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual void do_print() const;
  virtual void do_substitute_dependency (Score_element*,Score_element*);
  virtual void do_post_processing() ;
  virtual void do_pre_processing ();
  virtual void do_side_processing ();
};
#endif // VERTICAL_ALIGN_ITEM_HH
