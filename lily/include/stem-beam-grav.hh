/*
  stem-beam-grav.hh -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STEMBEAMGRAV_HH
#define STEMBEAMGRAV_HH
#include "engraver.hh"

/**
  TODO:
  override default_grouping if setting a n-plet
  
 */
class Stem_beam_engraver : public Engraver {
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
    DECLARE_MY_RUNTIME_TYPEINFO;
    Stem_beam_engraver();

protected:
    ~Stem_beam_engraver();
    virtual void set_feature(Feature dir_i_);
    virtual bool do_try_request(Request*);
    virtual void do_process_requests();
    virtual void acknowledge_element(Score_elem_info);
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();
};
#endif // STEMBEAMGRAV_HH
