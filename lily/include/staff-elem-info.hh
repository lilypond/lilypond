/*
  staff-elem-info.hh -- declare Staff_elem_info

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFFELEMINFO_HH
#define STAFFELEMINFO_HH

#include "proto.hh"
#include "varray.hh"

/// data container.
struct Staff_elem_info {
    Staff_elem * elem_p_;
    Request*req_l_;
    const Voice * voice_l_;
    Array<Request_register*> origin_reg_l_arr_;

    /* *** */
    Staff_elem_info(Staff_elem*, Request*);
    Staff_elem_info();
};

struct Staff_info {
    int *c0_position_i_;
    Staff_walker *walk_l_;
    const Time_description *time_c_l_;
    const Rhythmic_grouping *rhythmic_c_l_;
    bool break_allowed_b_;
};


struct Features {
    bool initialiser_b_;
    int direction_i_;
    
    Features();
    static Features dir(int);
};
#endif // STAFFELEMINFO_HH
