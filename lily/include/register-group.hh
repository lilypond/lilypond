/*
  registergroup.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REGISTERGROUP_HH
#define REGISTERGROUP_HH


#include "plist.hh"
#include "staff-elem-info.hh"
#include "register.hh"

/**
  Group a number of registers. Usually delegates everything to its contents.
  Postfix: group
  */
class Register_group_register : public Request_register {
protected:
    Pointer_list<Request_register*> reg_list_;
    virtual void do_print()const;
public:

    /**
      Junk #reg_l#.

      Pre:
        #reg_l# is in #reg_list_#
     */
    virtual void terminate_register(Request_register * reg_l);
    
   NAME_MEMBERS(Register_group_register);
    
    /**
      Remove #reg_l# from the list, and return it.
     */
    virtual Request_register * get_register_p(Request_register*reg_l);
    virtual void set_feature(Feature i);
    virtual bool acceptable_request_b(Request*)const;
    virtual void sync_features() ;
    virtual void pre_move_processing();
    virtual void post_move_processing();
    virtual void acknowledge_element(Staff_elem_info info);
    virtual bool try_request(Request*);
    virtual void process_requests();
    virtual ~Register_group_register();
    virtual void add(Request_register* reg_p);
    void add(Array<Request_register*> reg_p_arr);
    virtual bool contains_b(Request_register*)const;
};

#endif // REGISTERGROUP_HH


