/*
  registergroup.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REGISTERGROUP_HH
#define REGISTERGROUP_HH


#include "plist.hh"
#include "staffeleminfo.hh"
/**
  Group a number of registers. Usually delegates everything to its contents.
  */
class Register_group {
public:
    IPointerList<Request_register*> reg_list_;
    
    void set_dir(int i);
    bool acceptable_request_b(Request*);
    void pre_move_processing();
    void post_move_processing();
    void acknowledge_element(Staff_elem_info info);
    bool try_request(Request*);
    void process_requests();
    virtual ~Register_group();
    void add(Request_register* reg_p);
    bool contains_b(Request_register*);
//    bool contains_b(Register_group*);
};

#endif // REGISTERGROUP_HH


