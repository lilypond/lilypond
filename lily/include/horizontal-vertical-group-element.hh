/*   
  horizontal-vertical-group-element.hh -- declare Horizontal_vertical_group_element
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef HORIZONTAL_VERTICAL_GROUP_ELEM_HH
#define HORIZONTAL_VERTICAL_GROUP_ELEM_HH

#include "vertical-group-element.hh"
#include "horizontal-group-element.hh"

/** A class to treat a group of elements as a single entity. The
  dimensions are the unions of the dimensions of what it contains.
  Translation means translating the contents.
  */
class Horizontal_vertical_group_element : public Vertical_group_element, 
					  public Horizontal_group_element 
{  
protected:
public:
  Horizontal_vertical_group_element ();
  
  DECLARE_MY_RUNTIME_TYPEINFO;    
};


#endif /* HORIZONTAL_VERTICAL_GROUP_ELEM_HH */


