/*
  rest-collision.hh -- declare Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "lily-proto.hh"
#include "item.hh"

class Rest_collision : public Item {
    Link_array<Note_column> rest_l_arr_;
    Link_array<Note_column> ncol_l_arr_;
public:
    void add_column (Note_column*);
    
    Rest_collision();
protected:
    virtual void do_post_processing();
    virtual void do_pre_processing();
    virtual void do_print() const;
    virtual void do_substitute_element_pointer (Score_element*,Score_element*);
};
#endif // REST_COLLISION_HH
