/*   
  horizontal-group-elem.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
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
    virtual void remove_all();
    virtual Interval do_width() const;

public:
    DECLARE_MY_RUNTIME_TYPEINFO;
    virtual void add_element (Graphical_element*);
    virtual void remove_element (Graphical_element*);

};


#endif /* HORIZONTAL_GROUP_ELEM_HH */

