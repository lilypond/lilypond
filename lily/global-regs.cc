/*
  global-regs.cc -- implement 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "input-register.hh"
#include "debug.hh"
#include "register.hh"

struct Request_register_table_entry {
    String name_str_;
    Reg_ctor ctor_l_;
    Request_register_table_entry(String s, Reg_ctor f) {
	name_str_ =s;
	ctor_l_ = f;
    }
    Request_register_table_entry()
    {
	ctor_l_ =0;
    }
};

static Array<Request_register_table_entry> *reg_table=0;

void
add_request_register(String s, Reg_ctor f)
{
    if (!reg_table)
	reg_table = new Array<Request_register_table_entry>;
    
    reg_table->push(Request_register_table_entry(s, f));
}


Request_register*
get_nongroup_register_p(String s)
{
    for (int i=0; i < reg_table->size(); i++) {
	if ((*reg_table)[i].name_str_ == s)
	    return (*(*reg_table)[i].ctor_l_)();
    }
    error("Unknown register `" + s +"\'");
    return 0;
}
