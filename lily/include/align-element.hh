/*
  align-item.hh -- declare Align_elem

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VERTICAL_ALIGN_ITEM_HH
#define VERTICAL_ALIGN_ITEM_HH

#include "axis-group-element.hh"
#include "interval.hh"
#include "direction.hh"
#include "axes.hh"
#include "hash-table.hh"

/**
  Order elements top to bottom/left to right/right to left etc..

  TODO: implement padding.

  document usage of this.
 */
class Align_element : public virtual Axis_group_element {
  Hash_table<Score_element*,int> priority_i_hash_;
public:
  Interval threshold_interval_ ;

  /**
     Should high priorities be first or last?
   */
    
  Direction stacking_dir_;

  /**
     Which side to align?  -1: left side, 0: centered (around
     center_l_ if not nil, or around center of width), 1: right side

     URG. Unintuitive if stacking_dir_ == -1 
  */

  Direction align_dir_;
  
  Axis axis () const;
  Score_element * center_l_;
  
  Align_element ();
  void set_axis (Axis);
  void add_element (Score_element*);
  void add_element_priority (Score_element*, int);
  bool contains_b (Score_element const*) const;

  Score_element *get_elt_by_priority (int) const;
  int get_priority (Score_element*) const;
protected:
  void sort_elements ();
  virtual void do_print() const;
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual void do_post_processing() ;
  virtual void do_pre_processing ();
  virtual void do_side_processing ();
};
#endif // VERTICAL_ALIGN_ITEM_HH
