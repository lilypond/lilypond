/*
  elem-group.hh -- declare Element_group

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef ELEM_GROUP_HH
#define ELEM_GROUP_HH
#include "score-elem.hh"

/** A class to treat a group of elements as a single entity. The
  dimensions are the unions of the dimensions of what it contains.
  Translation means translating the contents.
  */
class Element_group : virtual Score_elem{  
protected:
    Link_array<Score_elem> elem_l_arr_;
    virtual void do_substitute_dependency(Score_elem* old, Score_elem* new_l);
    virtual Interval do_height()const;
    virtual Interval do_width()const;
    virtual void do_print() const ;
    virtual Element_group* elem_group() { return this; }
    
    
public:
    Element_group();
    Element_group(Element_group const&);
    NAME_MEMBERS(Element_group);
    virtual void translate(Offset);
    virtual void add_element(Score_elem*);
    virtual String TeX_string()const;
};

#endif // ELEM_GROUP_HH
