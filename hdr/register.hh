/*
  register.hh -- part of LilyPond

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef REGISTER_HH
#define REGISTER_HH

#include "proto.hh"
#include "varray.hh"
#include "request.hh"
#include "staffeleminfo.hh"


/**
  a struct which processes requests, and creates the #Staff_elem#s.
   Hungarian postfix: reg
  */
class Request_register {
    /**
      Warning: you can't copy a #Request_register#
      */
    Request_register(Request_register const &);
public:
    Complex_walker * walk_l_;
    Array<Request*> accepted_req_arr_;
    
    Request_register(Complex_walker*);
    Request_register();
    virtual ~Request_register(){}

    /**
      take note of item/spanner
      put item in spanner. Adjust local key; etc.

      Default: ignore the info
      */
    virtual void acknowledge_element(Staff_elem_info){}

    /**
      try to fit the request in this register

      @return
      false: not noted,  not taken.

      true: request swallowed. Don't try to put the request elsewhere.

      (may be we could use C++ exceptions.. :-)

      #Request_register::try_request# always returns false
      */
    virtual bool try_request(Request *req_l);
    
    /// make items/spanners with the requests you got
    virtual void process_request(){}

    /// typeset any spanners. Empty #accepted_req_arr_#
    void pre_move_processing();
    /// reset any appropriate data.
    void post_move_processing();
    virtual bool acceptable_request_b(Request*) const;    
    virtual void set_dir(int){}
protected:
    /// utility
    Paperdef * paper() const;


     /**
      invoke walker method to typeset element
      */
    void typeset_element(Staff_elem*elem_p);


    /**
      typeset a "command" item.
      If the column is not breakable, #pre_p# and #post_p# are junked
      */
    void typeset_breakable_item(Item * pre_p , Item * nobreak_p, Item * post_p);
    /** virtual, called by #pre_move_processing()#
      #Request_register::do_pre_move_process()# defaults to NOP
      */
    virtual void do_pre_move_process(){}
    /** virtual, called by #post_move_processing#,
      #Request_register::do_post_move_process()# defaults to NOP */
    virtual void do_post_move_process(){}
    /**
      Announce element to walker. Utility
      */
    void announce_element(Staff_elem_info);
};


#endif // REGISTER_HH

