/*
  stem-beam-reg.hh -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STEMBEAMREG_HH
#define STEMBEAMREG_HH
#include "register.hh"

/**
  TODO:
  override default_grouping if setting a n-plet
  
 */
class Stem_beam_register : public Request_register {
    Stem * stem_p_;
    Beam * beam_p_;
    Beam_req * beam_req_l_;
    Stem_req * stem_req_l_;
    Beam_req * start_req_l_;
    bool end_beam_b_;
    Rhythmic_grouping *current_grouping;
    int default_dir_i_;
public:
    /* *************** */
    NAME_MEMBERS();
    Stem_beam_register();

protected:
    ~Stem_beam_register();
    virtual void set_feature(Feature dir_i_);
    virtual bool do_try_request(Request*);
    virtual void do_process_requests();
    virtual void acknowledge_element(Score_elem_info);
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();
};
#endif // STEMBEAMREG_HH
