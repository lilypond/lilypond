/*
  register.hh -- part of LilyPond

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef REGISTER_HH
#define REGISTER_HH

#include "lily-proto.hh"
#include "varray.hh"
#include "request.hh"
#include "staff-elem-info.hh"
#include "staff-info.hh"

/**
  a struct which processes requests, and creates the #Staff_elem#s.
  It may use derived classes. Hungarian postfix: register
  */
class Request_register {
    friend class Register_group_register;
    /**
      You cannot copy a Request_register
     */
    Request_register(const Request_register&){}
protected:
    

    /// utility
    virtual Paper_def * paper() const;

    /**
      try to fit the request in this register

      @return
      false: not noted,  not taken.

      true: request swallowed. Don't try to put the request elsewhere.


      Default: always return false
      */
    virtual bool try_request(Request *req_l);
    
    /// make items/spanners with the requests you got
    virtual void process_requests(){}

    /** typeset any items/spanners. Default: do nothing
     */
    virtual void pre_move_processing(){}
    /** reset any appropriate data. Default: do nothing
     */
    virtual void post_move_processing(){}
   
    /**
      Is this request eligible to be processed? Default: return false.
     */
    virtual bool acceptable_request_b(Request*) const;

    /**
      typeset a "command" item. Default: pass on to daddy.
      If the column is not breakable, #pre_p# and #post_p# are junked
      */
    virtual void typeset_breakable_item(Item * pre_p ,
					Item * nobreak_p, Item * post_p);
    /**
      Invoke walker method to typeset element. Default: pass on to daddy.
      */
    virtual void typeset_element(Staff_elem*elem_p);
    
     /**
      take note of item/spanner
      put item in spanner. Adjust local key; etc.

      Default: ignore the info
      */
    virtual void acknowledge_element(Staff_elem_info) {}
    /**
      Announce element. Default: pass on to daddy. Utility
      */
    virtual void announce_element(Staff_elem_info);
    /**
      Set Feature of the register(s). Default: ignore Feature.
     */
    virtual void set_feature(Feature){}
    /**
      Does this equal or contain a certain register?
     */
    virtual bool contains_b(Request_register*reg_l)const;
    /**
      Get information on the staff. Default: ask daddy.
      */
    virtual Staff_info get_staff_info();
    
    
    virtual void do_print()const;  
public:
    /** Every Request_register (except for the 'top' which is directly
      inside the Staff_walker, is a element of a group.  */
    Register_group_register * daddy_reg_l_;

    Request_register();
    virtual ~Request_register(){}
    NAME_MEMBERS(Request_register);
    void print() const;
};

/**
  A macro to automate administration of registers.
 */
#define ADD_THIS_REGISTER(c)				\
struct c ## init {					\
    static Request_register * globalctor (){		\
	return new c;					\
    }							\
    c ## init () {					\
	add_request_register(c::static_name(), globalctor);	\
							\
    }							\
} _ ## c ## init;

typedef Request_register*(*Reg_ctor)(void);
void add_request_register(String s, Reg_ctor f);

#endif // REGISTER_HH

