/*
  elem-group.hh -- declare Horizontal_vertical_group_element

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef ELEM_GROUP_HH
#define ELEM_GROUP_HH

#include "score-elem.hh"
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
    virtual void add_element (Score_elem*);
    virtual void remove_element (Score_elem*);

};
/**
  Like Horizontal_group_element, but in X direction
 */
class Vertical_group_element : public virtual Axis_group_element {
protected:
    virtual Interval do_height() const;
    virtual void remove_all();

public:
    virtual void add_element (Score_elem*);
    virtual void remove_element (Score_elem*);
    DECLARE_MY_RUNTIME_TYPEINFO;
};

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
    virtual void add_element (Score_elem*);
    virtual void remove_element (Score_elem*);
    
    DECLARE_MY_RUNTIME_TYPEINFO;    
};

#endif // ELEM_GROUP_HH
