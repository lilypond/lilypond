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

/// 
struct Complex_staff : Staff {

    /* *************** */

    virtual void set_output(PScore *);
    virtual Staff_walker *get_walker_p();
};

#endif // COMPLEXSTAF_HH

