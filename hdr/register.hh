/*
  register.hh -- part of LilyPond

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef REGISTER_HH
#define REGISTER_HH
#include "proto.hh"
#include "sstack.hh"

/// data container.
struct Staff_elem_info {
    Staff_elem * elem_p_;
//    Array<const Request*> requestor_l_arr_;
    Request*req_l_;
    const Voice * voice_l_;
    Voice_group_registers * group_regs_l_;
    int group;
    Request_register * origin_reg_l_;

    /****/
    Staff_elem_info(Staff_elem*, Request*, Request_register*);
    Staff_elem_info();
};

/// Hungarian postfix: reg
struct Request_register {
    Complex_walker * walk_l_;
    Array<Request*> accepted_req_arr_;
    
    /****************/

    Request_register(Complex_walker*);
    Request_register();
    virtual ~Request_register(){}

    /// take note of item/spaanner
    virtual void acknowledge_element(Staff_elem_info){}
    /**
      put item in spanner. Adjust local key; etc.
      */
    
    ///
    virtual bool try_request(Request *req_l) =0;
    /**
      try to fit the request in this register

      RETURN
      false: request noted, but not taken.

      true: request swallowed, now owned by this

      (may be we could use C++ exceptions.. :-)
      */

    /// make items/spanners with the requests you got
    virtual void process_request()=0;

    /// typeset any spanners. Empty accepted_req_arr_
    void pre_move_processing();
    void post_move_processing();
    
protected:
    /// virtual, called by pre_move_process()
    virtual void do_pre_move_process(){}
    virtual void do_post_move_process(){}
};
/**
  a struct which processes requests, and creates the Staff_elems  
  */

struct Notehead_register : Request_register {
    Item* note_l_;
    /****************/
    Notehead_register(Complex_walker*);
    virtual bool try_request(Request *req_l) ;
    virtual void process_request();
    virtual void do_pre_move_process();
};

struct Slur_register : Request_register {
    sstack<Slur *> slur_l_stack_;
    Array<Slur*> end_slur_l_arr_;


    /****************/
    ~Slur_register();
    Slur_register(Complex_walker*);
    virtual bool try_request(Request*);
    virtual void process_request();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void do_pre_move_process();
};

struct Stem_beam_register : Request_register {
    Stem * stem_p_;
    Beam * beam_p_;
    Beam_req * beam_req_l_;
    Stem_req * stem_req_l_;
    bool end_beam_b_;
    Rhythmic_grouping *current_grouping;

    /****************/
    Stem_beam_register(Complex_walker*);
    ~Stem_beam_register();
    virtual bool try_request(Request*);
    virtual void process_request();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void do_pre_move_process();
    virtual void do_post_move_process();
};

#if 0
struct   Script_register : Request_register {
    Script * script_p_;
    /****************/
    Script_register(Complex_walker*);
    virtual bool try_request(Request*);
    virtual void process_request();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void do_pre_move_process();
};

struct Text_register:Request_register{
    Text_item * text_p_;

    /****************/
    Text_register(Complex_walker*);
    virtual bool try_request(Request*);
    virtual void process_request();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void do_pre_move_process();
};
#endif

struct Local_key_register : Request_register {
    Local_key_item* key_item_p_;

    /****************/
    
    virtual bool try_request(Request*);
    virtual void process_request();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void do_pre_move_process();
    Local_key_register(Complex_walker*);
};

#endif // REGISTER_HH

