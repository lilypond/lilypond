/*
  text-reg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef TEXTREG_HH
#define TEXTREG_HH
#include "register.hh"

class Text_register : public Request_register{
    Text_item * text_p_;
    Text_req * text_req_l_;
    int dir_i_;
    /* *************** */
protected:
    virtual void set_feature(Feature );
    virtual bool try_request(Request*);
    virtual void process_requests();
    virtual void pre_move_processing();
    virtual void post_move_processing();
    virtual void acknowledge_element(Staff_elem_info);
public:
    Text_register();
    NAME_MEMBERS(Text_register);
};

#endif // TEXTREG_HH
