/*
  rest-collision-grav.hh -- declare Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REST_COLLISION_GRAV_HH
#define REST_COLLISION_GRAV_HH

#include "varray.hh"
#include "engraver.hh"

class Rest_collision_engraver : public Request_engraver {
    Rest_collision* rest_collision_p_;
    Array< Collision *> collision_l_arr_;
protected:
    virtual void acknowledge_element(Score_elem_info);
    virtual void do_print() const;
    virtual void do_pre_move_processing();
public:
    Rest_collision_engraver();
    NAME_MEMBERS();
};
#endif // REST_COLLISION_GRAV_HH
