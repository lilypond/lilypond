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
    Voice const * voice_l_;
    Array<Request_register*> origin_reg_l_arr_;

    /* *** */
    Staff_elem_info(Staff_elem*, Request*);
    Staff_elem_info();
};

/// struct to pass staff info along a Request_register hierarchy.
struct Staff_info {
    int *c0_position_i_;
    Staff_walker *walk_l_;

    /// when is now?
    Time_description const *time_C_;
    Rhythmic_grouping const *rhythmic_C_;
    bool break_allowed_b_;
};


struct Features {
    bool initialiser_b_;
    int direction_i_;
    
    Features();
    static Features dir(int);
};
#endif // STAFFELEMINFO_HH
