/*
  horizontal-group-item.hh -- declare Horizontal_group_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HORIZONTAL_GROUP_ITEM_HH
#define HORIZONTAL_GROUP_ITEM_HH

#include "elem-group.hh"
#include "item.hh"

class Horizontal_group_item : public Item, public Horizontal_group {
    DECLARE_MY_RUNTIME_TYPEINFO;
    SCORE_ELEM_CLONE(Horizontal_group_item);
protected:
    virtual void do_breakable_col_processing();
    void OK()const;
    virtual void do_print() const { Elbement_group::do_print(); }
};

#endif // HORIZONTAL_GROUP_ITEM_HH
