/*   
  vertical-group-element.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef VERTICAL_GROUP_ELEM_HH
#define VERTICAL_GROUP_ELEM_HH

#include "axis-group-element.hh"
/**
  Like Horizontal_group_element, but in X direction
 */
class Vertical_group_element : public virtual Axis_group_element {
protected:
  virtual Interval do_height() const;

public:
  Vertical_group_element () ;
  
};

#endif /* VERTICAL_GROUP_ELEM_HH */

