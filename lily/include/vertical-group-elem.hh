/*   
  vertical-group-elem.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
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
    virtual void remove_all();

public:
    virtual void add_element (Graphical_element*);
    virtual void remove_element (Graphical_element*);
    DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif /* VERTICAL_GROUP_ELEM_HH */

