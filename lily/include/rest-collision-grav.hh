/*
  rest-collision-grav.hh -- declare Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REST_COLLISION_GRAV_HH
#define REST_COLLISION_GRAV_HH

#include "varray.hh"
#include "engraver.hh"

class Rest_collision_engraver : public Engraver {
    Rest_collision* rest_collision_p_;

    void make_collision();
protected:
    virtual void acknowledge_element (Score_elem_info);
    virtual void do_print() const;
    virtual void do_pre_move_processing();
public:
    Rest_collision_engraver();
    DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // REST_COLLISION_GRAV_HH
