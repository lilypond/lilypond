/*
  graphical-element.hh -- declare Graphical_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef GRAPHICAL_ELEMENT_HH
#define GRAPHICAL_ELEMENT_HH

#include "offset.hh"
#include "lily-proto.hh"
#include "interval.hh"
#include "virtual-methods.hh"
#include "dimension-cache.hh"



/** The 2d geometric aspects of a score-element.  It was put in a
  separate class, because Score_element got quite big.
  */
class Graphical_element 
{
  void init ();
public:
  Dimension_cache dim_cache_[NO_AXES];

  char const * name () const;
  void set_empty (bool);
  bool empty_b () const;
  
  Graphical_element ();
  Graphical_element (Graphical_element const&);
  virtual ~Graphical_element ();
  
  void invalidate_cache (Axis);
  Interval extent (Axis) const;
 
  /**
    translate the symbol. The symbol does not have to be created yet. 
    */
  void translate (Offset);
  /**
    translate in one direction
    */
    
  void translate_axis (Real, Axis);

  Real relative_coordinate (Dimension_cache*group, Axis) const;
  Offset absolute_offset() const;
  Real absolute_coordinate (Axis) const;
  /**
    Find the group-element which has both #this# and #s#
   */
  Dimension_cache*common_group (Graphical_element const* s, Axis a) const;
  Dimension_cache*common_group (Link_array<Graphical_element> elems, Axis a) const;
  
  Graphical_element *parent_l (Axis a) const;
  
  void unlink ();
  void junk_links ();
  virtual void do_print () const;
protected:
  virtual Interval do_height () const=0;
  virtual Interval do_width () const=0;
};

#endif // GRAPHICAL_ELEMENT_HH

