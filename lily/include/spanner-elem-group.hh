/*
  spanner-elem-group.hh -- declare Spanner_elem_group

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPANNER_ELEM_GROUP_HH
#define SPANNER_ELEM_GROUP_HH

#include "spanner.hh"
#include "elem-group.hh"

class Spanner_elem_group : public Spanner, public Element_group {
    
protected:
    virtual Interval do_width()const;
    virtual void do_print() const;
    SPANNER_CLONE(Spanner_elem_group)
    NAME_MEMBERS();
};
#endif // SPANNER_ELEM_GROUP_HH
