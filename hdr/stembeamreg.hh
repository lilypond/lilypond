/*
  stembeamreg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STEMBEAMREG_HH
#define STEMBEAMREG_HH
#include "register.hh"

struct Stem_beam_register : Request_register {
    Stem * stem_p_;
    Beam * beam_p_;
    Beam_req * beam_req_l_;
    Stem_req * stem_req_l_;
    Beam_req * start_req_l_;
    bool end_beam_b_;
    Rhythmic_grouping *current_grouping;
    int default_dir_i_;
    
    /* *************** */
    Stem_beam_register(Complex_walker*);
    ~Stem_beam_register();
    void set_dir(int dir_i_);
    virtual bool try_request(Request*);
    virtual void process_request();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void do_pre_move_process();
    virtual void do_post_move_process();
};
#endif // STEMBEAMREG_HH
