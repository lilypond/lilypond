/*
  staffeleminfo.hh -- declare Staff_elem_info

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFFELEMINFO_HH
#define STAFFELEMINFO_HH

#include "proto.hh"

/// data container.
struct Staff_elem_info {
    Staff_elem * elem_p_;
    Request*req_l_;
    const Voice * voice_l_;
    Voice_group_registers * group_regs_l_; 
    Request_register * origin_reg_l_;

    /* *** */
    Staff_elem_info(Staff_elem*, Request*, Request_register*);
    Staff_elem_info();
};

#endif // STAFFELEMINFO_HH
