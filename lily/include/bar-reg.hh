/*
  bar-reg.hh -- declare Bar_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef BARREG_HH
#define BARREG_HH
#include "register.hh"

/**
  generate bars. Either user ("|:"), or default (new measure)
  */
class Bar_register : public Request_register {
    Bar_req * bar_req_l_;
    Bar * bar_p_;
public:
    Bar_register();
    NAME_MEMBERS();

     
protected:
    virtual bool do_try_request(Request *req_l);
    virtual void do_process_requests();
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();
};

#endif // BARREG_HH
