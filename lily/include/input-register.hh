/*
  input-register.hh -- declare Input_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef INPUT_REGISTER_HH
#define INPUT_REGISTER_HH

#include "plist.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "input.hh"
#include "string.hh"
#include "varray.hh"

struct Input_register : Input { 
    Pointer_list<Input_register*> contains_ireg_p_list_;
    Array<String> consists_str_arr_;
    Array<String> alias_str_arr_;
    String name_str_;

    void add(Input_register *);
    bool is_name_b(String);
    bool accept_req_b();
    bool accepts_b(String);
    void print() const;
    Register_group_register * get_group_register_p();
    Input_register * get_default_ireg_l();
    Input_register * recursive_find(String nm);
    Input_register * find_ireg_l(String nm);    
};


void add_global_input_register(Input_register* ireg_p);
Input_register*lookup_reg(String);
Request_register*get_register_p(String s);

#endif // INPUT_REGISTER_HH
