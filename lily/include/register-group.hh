/*
  registergroup.hh -- declare Register_group_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REGISTERGROUP_HH
#define REGISTERGROUP_HH

#include "parray.hh"
#include "plist.hh"
#include "score-elem-info.hh"
#include "register.hh"
#include "acceptor.hh"


/**
  Group a number of registers. Usually delegates everything to its contents.
  Postfix: group
  */
class Register_group_register : public Request_register, public virtual Acceptor {
protected:
    
    Pointer_list<Request_register*> reg_list_;
    Link_array<Register_group_register> group_l_arr_;
    Link_array<Request_register> nongroup_l_arr_;
    
    Array<Score_elem_info> announce_info_arr_;
    
    virtual void do_print()const;

    virtual bool removable_b()const;

public:
    Input_register * ireg_l_;
    void check_removal();
    Register_group_register();
    ~Register_group_register();
    

    
    /**
      Junk #reg_l#.
      Pre:
      #reg_l# is in #reg_list_#
     */
    virtual void terminate_register(Request_register * reg_l);
    
   NAME_MEMBERS();
    
    /**
      Remove #reg_l# from the list, and return it.
     */
    virtual Request_register * remove_register_p(Request_register*reg_l);
    virtual void set_feature(Feature i);
    virtual void sync_features() ;
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();

    virtual void do_removal_processing();
    virtual bool do_try_request(Request*);
    virtual void do_process_requests();

    virtual Staff_info get_staff_info()const;
    
    virtual Register_group_register * find_register_l(String name,String id);
    virtual void do_announces();    
    virtual void announce_element(Score_elem_info);

        
    virtual void add(Request_register* reg_p);

    virtual bool contains_b(Request_register*)const;

    virtual Acceptor* find_get_acceptor_l(String name, String id);
    virtual Acceptor * get_default_interpreter();
    /**
      Go up in the tree. default: choose next parent
     */
    Acceptor * ancestor_l(int l=1);
    int depth_i() const;

};

#endif // REGISTERGROUP_HH


