/*
  horizontal-group-item.hh -- declare Horizontal_group_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HORIZONTAL_GROUP_ITEM_HH
#define HORIZONTAL_GROUP_ITEM_HH

#include "elem-group.hh"
#include "axis-group-item.hh"

class Horizontal_group_item : public Axis_group_item, public Horizontal_group_element {
protected:
    virtual void remove_all() { Horizontal_group_element::remove_all (); }
    virtual void do_print() const;
public:
    virtual void add_element (Score_elem*e) { Horizontal_group_element::add_element (e); }
    virtual void remove_element (Score_elem*e) { Horizontal_group_element::remove_element (e); }
    DECLARE_MY_RUNTIME_TYPEINFO;
    SCORE_ELEM_CLONE(Horizontal_group_item);
  

};

#endif // HORIZONTAL_GROUP_ITEM_HH
