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
    void split_bar(Bar *& pre, Bar * no, Bar * &post);
public:
    Bar_req * bar_req_l_;
    Bar * bar_p_;
 
    virtual bool try_request(Request *req_l);
    virtual void process_requests();
    virtual void pre_move_processing();
    virtual void post_move_processing();
    Bar_register();
    NAME_MEMBERS();
};

#endif // BARREG_HH
