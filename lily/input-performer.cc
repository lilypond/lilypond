/*
  input-performer.cc -- implement Input_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
           Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "debug.hh"
#include "performer.hh"
#include "input-performer.hh"
#include "parray.hh"
#include "input-performer.hh"
#include "performer-group-performer.hh"

bool
Input_performer::is_name_b(String n)
{
    for (int i=0; i < alias_str_arr_.size(); i++)
	if (alias_str_arr_[i] == n)
	    return true;
    return false;
}

void
Input_performer::print() const
{
#ifndef NPRINT
    mtor << "type " << type_str_;
    mtor << "Consists of ";
    for (int i=0; i< consists_str_arr_.size(); i++)
	mtor << consists_str_arr_[i] << ',';
    mtor << "contains " ;
    for (PCursor<Input_performer*> i(contains_iperf_p_list_.top()); i.ok(); i++) 
	i->print();
#endif 
}



Input_performer *
Input_performer::recursive_find(String nm)
{
    if ( is_name_b( nm) )
	return this;

    Input_performer * r =0;
    PCursor<Input_performer*> i(contains_iperf_p_list_.top());
    for (; !r &&i.ok(); i++) {
	if (i->recursive_find(nm))
	    r = i.ptr();
    }

    return r;
}

Input_performer *
Input_performer::find_iperf_l(String nm)
{
    for (PCursor<Input_performer*> i(contains_iperf_p_list_.top()); i.ok(); i++) 
	if (i->is_name_b( nm))
	    return i;

    return 0;
}


Performer_group_performer *
Input_performer::get_group_performer_p()
{
    Performer_group_performer * perf_p = (Performer_group_performer*)
	get_performer_p(type_str_);

    for (int i=0; i < consists_str_arr_.size(); i++) {
	perf_p->add( get_performer_p( consists_str_arr_[i]) );
    }
    perf_p -> iperf_l_ = this;
    return perf_p;
}


bool
Input_performer::accept_req_b()
{
    return ! contains_iperf_p_list_.size();
}

void
Input_performer::add(Input_performer *ip)
{
    contains_iperf_p_list_.bottom().add(ip);
}

Input_performer*
Input_performer::get_default_iperf_l()
{
    if ( contains_iperf_p_list_.size() )
	return contains_iperf_p_list_.top();
    else
	return 0;
}


Input_performer_list::Input_performer_list(Input_performer_list const &s)
{
    for (PCursor<Input_performer*> pc(s); pc.ok(); pc++) {
	Input_performer *q = pc;
	Input_performer *p=new Input_performer(*q) ; 
	bottom().add(p);
    }
}
