/*
  tie-reg.hh -- declare Tie_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef TIE_REG_HH
#define TIE_REG_HH

#include "register.hh"

class Tie_register : public Request_register {
    Tie * end_tie_p_;
    Tie * tie_p_;
    Moment end_mom_;
    Tie_req * req_l_;
    int dir_i_;
    Tie_req *end_req_l_;
    Melodic_req * end_melodic_req_l_;
    Melodic_req  * melodic_req_l_;
    
protected:
    virtual ~Tie_register();
    virtual void acknowledge_element(Staff_elem_info);
    virtual bool try_request(Request*);
    virtual bool acceptable_request_b(Request*);
    virtual void sync_features();
    virtual void process_requests();
    virtual void post_move_processing();
    virtual void pre_move_processing();
    virtual void set_feature(Feature);
public:
    Tie_register();
    NAME_MEMBERS(Tie_register);
};

#endif // TIE_REG_HH
