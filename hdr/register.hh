/*
  register.hh -- part of LilyPond

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef REGISTER_HH
#define REGISTER_HH
#include "proto.hh"
#include "varray.hh"
#include "request.hh"

/// data container.
struct Staff_elem_info {
    Staff_elem * elem_p_;
    Request*req_l_;
    const Voice * voice_l_;
    Voice_group_registers * group_regs_l_; 
    Request_register * origin_reg_l_;

    /* *** */
    Staff_elem_info(Staff_elem*, Request*, Request_register*);
    Staff_elem_info();
};

/// Hungarian postfix: reg
/**
  a struct which processes requests, and creates the Staff_elems  
  */
struct Request_register {
    Complex_walker * walk_l_;
    Array<Request*> accepted_req_arr_;
    
    /* *************** */
    /**
      Warning: you can't copy a Request_register
      */
    Request_register(Request_register const &);
    Request_register(Complex_walker*);
    Request_register();
    virtual ~Request_register(){}

    /** take note of item/spaanner
	put item in spanner. Adjust local key; etc.
      */
    virtual void acknowledge_element(Staff_elem_info){}
    /**
      Announce element to  walker
      */
    void announce_element(Staff_elem_info);

    /**
      invoke walker method to typeset element
      */
    void typeset_element(Staff_elem*elem_p);
    
    /**
      try to fit the request in this register

      @return
      false: not noted,  not taken.

      true: request swallowed. Don't try to put elsewhere

      (may be we could use C++ exceptions.. :-)
      */
    virtual bool try_request(Request *req_l) =0;
    
    /// make items/spanners with the requests you got
    virtual void process_request()=0;
    Paperdef * paper() const;
    /// typeset any spanners. Empty accepted_req_arr_
    void pre_move_processing();
    void post_move_processing();
    virtual void set_dir(int){}
protected:
    /// virtual, called by pre_move_process()
    virtual void do_pre_move_process(){}
    virtual void do_post_move_process(){}
};


#endif // REGISTER_HH

