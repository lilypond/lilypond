/*
  barreg.hh -- declare Bar_register

  source file of the LilyPond music typesetter

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
    virtual void process_request();
    virtual void do_pre_move_process();
    virtual void do_post_move_process();
    Bar_register(Complex_walker*);
};

#endif // BARREG_HH
