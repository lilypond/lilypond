/*
  input-register.cc -- implement Input_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "debug.hh"
#include "register.hh"
#include "input-register.hh"
Input_register::~Input_register()
{}

Input_register::Input_register()
{
}

bool
Input_register::group_b()const
{
    return ireg_list_.size();
}

Array<String>
Input_register::get_nongroups_str_arr()const
{
    Array<String> s_arr;
    for (iter_top(ireg_list_, i); i.ok(); i++) {
	if (!i->group_b())
	    s_arr.push(i->name_str_);
    }
    return s_arr;
}

Input_register*
Input_register::get_ireg_l(String nm)const
{
    for (iter_top(ireg_list_, i); i.ok(); i++) {
	if (i->name_str_ == nm)
	    return i;
    }
    return 0;
}
Array<Request_register*>
Input_register::get_nongroup_p_arr() const return a;
{
    Array<String> sa(get_nongroups_str_arr());
    for (int i=0; i < sa.size(); i++)
	a.push(get_nongroup_register_p(sa[i]));
}

void
Input_register::add(Input_register *p)
{
    ireg_list_.bottom().add(p);
}

void
Input_register::print() const
{
#ifndef NPRINT
    mtor << name_str_ << " { ";
    for (iter_top(ireg_list_, i); i.ok(); i++)
	i->print();
    mtor <<" }\n";
#endif 
}
	

Input_register::Input_register(Input_register const&s)
    : Input(s)
{
    name_str_ = s.name_str_;
    for (iter_top(s.ireg_list_, i); i.ok(); i++)
	add(new Input_register(*i.ptr()));
}
