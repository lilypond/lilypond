/*
  horizontal-align-item.hh -- declare Horizontal_align_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HORIZONTAL_ALIGN_ITEM_HH
#define HORIZONTAL_ALIGN_ITEM_HH
#include "elem-group.hh"
#include "item.hh"

/**
  Order elems left to right.

  TODO: insert(order, elem)
  */
class Horizontal_align_item :  public Item {
protected:
    Link_array<Item> item_l_arr_;
    Array<int> priority_i_arr_;
    int align_i_;
    
public:
    NAME_MEMBERS();
    SCORE_ELEM_CLONE(Horizontal_align_item)
    void add(Item*, int p);
    Horizontal_align_item();
protected:
    virtual void do_substitute_dependency(Score_elem * , Score_elem *);
    /// do calculations before determining horizontal spacing
    virtual void do_pre_processing();
    virtual void do_print()const;
    virtual Interval do_width()const;
    bool contains_b(Item*)const;
};
#endif // HORIZONTAL_ALIGN_ITEM_HH
