/*
  staff-sym-reg.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_SYM_REG_HH
#define STAFF_SYM_REG_HH
#include "register.hh"
#include "moment.hh"

/**
  Manage the staff symbol.
 */
class Staff_sym_register : public Request_register { 
    Staff_symbol *span_p_;
    Moment last_mom_;
public:
    ~Staff_sym_register();
    Staff_sym_register();
    NAME_MEMBERS(Staff_sym_register);
    virtual void pre_move_processing();
    virtual void post_move_processing();
};
#endif // STAFF_SYM_REG_HH
