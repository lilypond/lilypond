/*
  dynamic-reg.hh -- declare Dynamic_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef DYNAMIC_REG_HH
#define DYNAMIC_REG_HH

#include "register.hh"

class Dynamic_register : public Request_register {
    int dir_i_;
    Text_item * dynamic_p_;
    Crescendo * to_end_cresc_p_;
    Crescendo * cresc_p_;
    Span_dynamic_req * cresc_req_l_;
    Array<Dynamic_req*> dynamic_req_l_arr_;
    /* ************** */
public:
    Dynamic_register();
    ~Dynamic_register();
    NAME_MEMBERS(Dynamic_register);
protected:
    virtual void acknowledge_element(Staff_elem_info);
    virtual bool try_request(Request *req_l);
    virtual void process_requests();
    virtual void pre_move_processing();
    virtual void post_move_processing();
    virtual bool acceptable_request_b(Request*) const;
    virtual void set_feature(Feature);
};

#endif // DYNAMIC_REG_HH
