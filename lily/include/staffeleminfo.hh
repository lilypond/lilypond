/*
  staffeleminfo.hh -- declare Score_elem_info

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFFELEMINFO_HH
#define STAFFELEMINFO_HH

#include "proto.hh"

/// data container.
struct Score_elem_info {
    Score_elem * elem_p_;
    Request*req_l_;
    Voice const * voice_l_;
    Voice_group_registers * group_regs_l_; 
    Request_register * origin_reg_l_;

    /* *** */
    Score_elem_info(Score_elem*, Request*, Request_register*);
    Score_elem_info();
};

#endif // STAFFELEMINFO_HH
