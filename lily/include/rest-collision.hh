/*
  rest-collision.hh -- declare Rest_collision

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "lily-proto.hh"
#include "item.hh"

class Rest_collision : public Item {
    Link_array<Rest_column> rest_l_arr_;
    Link_array<Note_column> ncol_l_arr_;
public:
    void add(Rest_column*);
    void add(Collision*);
    NAME_MEMBERS();
protected:
    virtual void do_post_processing();
    virtual void do_substitute_dependency(Score_elem*,Score_elem*);
};
#endif // REST_COLLISION_HH
