/*
  text-reg.hh -- part of GNU LilyPond

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
    virtual bool do_try_request(Request*);
    virtual void do_process_requests();
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();
    virtual void acknowledge_element(Score_elem_info);
public:
    Text_register();
    NAME_MEMBERS();
};

#endif // TEXTREG_HH
