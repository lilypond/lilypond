/*
  input-register.cc -- implement Input_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "register.hh"
#include "input-register.hh"
#include "parray.hh"
#include "input-register.hh"
#include "register-group.hh"

void
Input_register::print() const
{
#ifndef NPRINT
    mtor << "name " << name_str_;
    mtor << "Consists of ";
    for (int i=0; i< consists_str_arr_.size(); i++)
	mtor << consists_str_arr_[i] << ',';
    mtor << "contains " ;
    for (iter(contains_ireg_p_list_.top(), i); i.ok(); i++) 
	i->print();
#endif 
}

/*
  UGH. Global.
 */
Link_array<Input_register> iregs_p_arr;

void
add_global_input_register(Input_register *reg_p)
{
    iregs_p_arr.push(reg_p);
}

Input_register *
lookup_reg(String nm)
{
    for (int i=0; i < iregs_p_arr.size(); i++)
	if (iregs_p_arr[i]->name_str_ == nm)
	    return iregs_p_arr[i];

    error("can't find reg `" + nm + "'");
}

Input_register *
Input_register::recursive_find(String nm)
{
    if ( nm == name_str_)
	return this;

    Input_register * r =0;
    for (iter(contains_ireg_p_list_.top(), i); !r &&i.ok(); i++)
	r = i->recursive_find(nm);

    return r;
}
Input_register *
Input_register::find_ireg_l(String nm)
{
    for (iter(contains_ireg_p_list_.top(), i); i.ok(); i++)
	if (i->name_str_ == nm)
	    return i;

    return 0;
}


Register_group_register *
Input_register::get_group_register_p()
{
    Register_group_register * reg_p = (Register_group_register*)
	get_register_p(name_str_);

    
     
    for (int i=0; i < consists_str_arr_.size(); i++) {
	reg_p->add( get_register_p( consists_str_arr_[i]) );
    }
    reg_p -> ireg_l_ = this;
    return reg_p;
}


bool
Input_register::accept_req_b()
{
    return ! contains_ireg_p_list_.size();
}

void
Input_register::add(Input_register *ip)
{
    contains_ireg_p_list_.bottom().add(ip);
}

Input_register*
Input_register::get_default_ireg_l()
{
    return contains_ireg_p_list_.top();
}
