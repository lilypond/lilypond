/*
  rest-collision.hh -- declare Rest_collision

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "lily-proto.hh"
#include "item.hh"

class Rest_collision : public Item {
    Array<Rest_column *> rest_l_arr_;
    Array<Note_column *> ncol_l_arr_;
public:
    void add(Rest_column*);
    void add(Collision*);
    NAME_MEMBERS(Rest_collision);
protected:
    virtual void do_post_processing();
};
#endif // REST_COLLISION_HH
