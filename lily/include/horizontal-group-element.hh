/*   
  horizontal-group-element.hh -- declare Horizontal_group_element	
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef HORIZONTAL_GROUP_ELEM_HH
#define HORIZONTAL_GROUP_ELEM_HH

#include "axis-group-element.hh"

/**
  Treat a group of elements a unity in horizontal sense .
  A column is a typical Vertical_group.
 */
class Horizontal_group_element : public virtual Axis_group_element {
protected:
  virtual Interval do_width() const;

public:
  Horizontal_group_element ();
  DECLARE_MY_RUNTIME_TYPEINFO;
};


#endif /* HORIZONTAL_GROUP_ELEM_HH */

