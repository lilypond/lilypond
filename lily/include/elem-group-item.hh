/*
  elem-group-item.hh -- declare Horizontal_vertical_group_item
  
  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef ELEM_GROUP_ITEM_HH
#define ELEM_GROUP_ITEM_HH

#include "elem-group.hh"
#include "item.hh"

class Horizontal_vertical_group_item  : public Item, public Horizontal_vertical_group {
public:
    DECLARE_MY_RUNTIME_TYPEINFO;
    SCORE_ELEM_CLONE(Horizontal_vertical_group_item);
protected:
    virtual void do_print() const;
};

#endif // ELEM_GROUP_ITEM_HH
