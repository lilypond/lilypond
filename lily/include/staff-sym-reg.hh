/*
  staff-sym-reg.hh -- declare 

  source file of the GNU LilyPond music typesetter

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
public:
    Staff_sym_register();
    NAME_MEMBERS();
protected:
    virtual void do_process_requests();
    virtual void fill_staff_info(Staff_info&);
    virtual void do_removal_processing();
    virtual void do_creation_processing();
	
};
#endif // STAFF_SYM_REG_HH
