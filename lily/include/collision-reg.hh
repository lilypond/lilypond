/*
  collision-reg.hh -- declare Collision_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef COLLISION_REG_HH
#define COLLISION_REG_HH

#include "register.hh"

class Collision_register : public Request_register {
    Collision* col_p_;

protected:
    virtual void acknowledge_element(Score_elem_info);
    virtual void pre_move_processing();
public:
    Collision_register();
    NAME_MEMBERS();
};
#endif // COLLISION_REG_HH
