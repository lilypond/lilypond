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

/** The 2d geometric aspects of a score-element.
  */
class Graphical_element 
{
  void init ();
public:
  Dimension_cache *dim_cache_[NO_AXES];

  /**
     Set this if anyone points to me, or if I point to anyone.
   */
  bool used_b_;
  
  char const * name () const;
  /**
     Set empty in direction a1 and a2.  If an argument is NO_AXES, it is ignored.
   */
  void set_empty (bool b, Axis a1 = NO_AXES, Axis a2 = NO_AXES);
  bool empty_b (Axis a1);
  Graphical_element ();
  Graphical_element (Graphical_element const&);
  virtual ~Graphical_element ();
  
  void invalidate_cache (Axis);
  Interval extent (Axis) const;
 
  /**
    translate the symbol.
    */
  void translate (Offset);
  /**
    translate in one direction
    */
    
  void translate_axis (Real, Axis);

  Real relative_coordinate (Graphical_element const* refp, Axis) const;
  /**
    Find the group-element which has both #this# and #s#
   */
  Graphical_element*common_refpoint (Graphical_element const* s, Axis a) const;
  Graphical_element*common_refpoint (Link_array<Graphical_element> elems, Axis a) const;

  /**
     Set the  parent refpoint of THIS to E
   */
  void set_parent (Graphical_element* e, Axis);
  
  Graphical_element *parent_l (Axis a) const;
  
  virtual void do_print () const;
  virtual void print () const;  
};

#endif // GRAPHICAL_ELEMENT_HH

