/*
  staff-sym-grav.hh -- declare 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_SYM_GRAV_HH
#define STAFF_SYM_GRAV_HH
#include "engraver.hh"
#include "moment.hh"

/**
  Manage the staff symbol.
 */
class Staff_sym_engraver : public Engraver { 
    Staff_symbol *span_p_;
public:
    Staff_sym_engraver();
    NAME_MEMBERS();
protected:
    virtual void do_process_requests();
    virtual void fill_staff_info(Staff_info&);
    virtual void do_removal_processing();
    virtual void do_creation_processing();
	
};
#endif // STAFF_SYM_GRAV_HH
