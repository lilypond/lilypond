/*
  input-register.hh -- declare Input_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef INPUT_REGISTER_HH
#define INPUT_REGISTER_HH

#include "plist.hh"
#include "string.hh"
#include "proto.hh"
#include "input.hh"

struct Input_register : Input { 
    IPointerList<Input_register*> ireg_list_;
    String name_str_;
    
    void add(Input_register*);
    Input_register();
    bool group_b() const;
    ~Input_register();
    /** Get an Input_register with a certain name

      @return 0 if not found.
      */
    Input_register *get_ireg_l(String name) const;
    Input_register(Input_register const&);
    /** 
      The names of all non -groups.
      */
    Array<String> get_nongroups_str_arr() const;
    void print() const;
    /**
      Construct the registers.
     */
    Array<Request_register*> get_nongroup_p_arr()const;
};

Request_register * get_nongroup_register_p(String);

#endif // INPUT_REGISTER_HH
