/*   
  horizontal-vertical-group-elem.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#ifndef HORIZONTAL_VERTICAL_GROUP_ELEM_HH
#define HORIZONTAL_VERTICAL_GROUP_ELEM_HH

#include "vertical-group-elem.hh"
#include "horizontal-group-elem.hh"

/** A class to treat a group of elements as a single entity. The
  dimensions are the unions of the dimensions of what it contains.
  Translation means translating the contents.
  */
class Horizontal_vertical_group_element : public Vertical_group_element, 
				  public Horizontal_group_element 
{  
protected:
    virtual void remove_all();
public:
    virtual void add_element (Graphical_element*);
    virtual void remove_element (Graphical_element*);
    
    DECLARE_MY_RUNTIME_TYPEINFO;    
};


#endif /* HORIZONTAL_VERTICAL_GROUP_ELEM_HH */


