/*
  complexstaff.hh -- declare Complex_staff

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef COMPLEXSTAF_HH
#define COMPLEXSTAF_HH


#include "key.hh"
#include "stcol.hh"
#include "staff.hh"
#include "staffwalker.hh"
#include "complexcolumn.hh"


/// Complex  staff: one voicegroup  at a time
struct Complex_staff : Staff {
    /// indirection to the PStaff.
    PStaff *pstaff_l_;

    /* *************** */
    Staff_column*create_col();    
    virtual void set_output(PScore *);
    virtual Staff_walker *get_walker_p();
    Complex_staff();
};

#endif // COMPLEXSTAF_HH

