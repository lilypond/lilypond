/*
  graphical-element.hh -- declare Graphical_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef GRAPHICAL_ELEMENT_HH
#define GRAPHICAL_ELEMENT_HH

#include "offset.hh"
#include "lily-proto.hh"
#include "interval.hh"
#include "virtual-methods.hh"

/** The 2d geometric aspects of a score-element.  It was put in a
  separate class, because Score_elem got quite big.

  */
class Graphical_element 
{
  void init ();
  /** Is this element dimensionless?.
    This is private to guarantee correctness of the cache
   */
  bool empty_b_;
  
public:
  void set_empty (bool);
  bool empty_b () const;
  
  DECLARE_MY_RUNTIME_TYPEINFO;
  /**
    This is  needed, because #output# may still be
    NULL.
    */
  Offset offset_;
  
  Axis_group_element * axis_group_l_a_[NO_AXES];
  Interval cached_dimension_a_[NO_AXES];
  bool cached_valid_b_a_[NO_AXES];
  
  Graphical_element ();
  Graphical_element (Graphical_element const&);
  virtual ~Graphical_element ();
  
  void invalidate_cache (Axis);
  Interval extent (Axis) const;
  Interval width() const;
  Interval height() const;
 
  /**
    translate the symbol. The symbol does not have to be created yet. 
    */
  void translate (Offset);
  /**
    translate in one direction
    */
    
  void translate_axis (Real, Axis);

  Real relative_coordinate (Axis_group_element*group, Axis) const;
  Offset absolute_offset() const;
  Real absolute_coordinate (Axis) const;
  /**
    Find the group-element which has both #this# and #s#
   */
  Axis_group_element*common_group (Graphical_element const* s, Axis a) const;
  void unlink ();
  void junk_links ();
  void print () const;
protected:
  virtual Interval do_height () const=0;
  virtual Interval do_width () const=0;
};

#endif // GRAPHICAL_ELEMENT_HH

