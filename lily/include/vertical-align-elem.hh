/*
  vertical-align-item.hh -- declare Vertical_align_elem

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VERTICAL_ALIGN_ITEM_HH
#define VERTICAL_ALIGN_ITEM_HH

#include "score-elem.hh"

/**
  Order elements top to bottom.
 */
class Vertical_align_element : virtual public Score_elem {
    Link_array<Score_elem> elem_l_arr_;
public:
    void add (Score_elem*);
    bool contains_b (Score_elem const*) const;
    Vertical_align_element();
    DECLARE_MY_RUNTIME_TYPEINFO;
protected:
    virtual void do_substitute_dependency (Score_elem*,Score_elem*);
    virtual void do_post_processing() ;
};
#endif // VERTICAL_ALIGN_ITEM_HH
