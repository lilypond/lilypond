/*
  input-engraver.cc -- implement Input_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "engraver.hh"
#include "input-engraver.hh"
#include "parray.hh"
#include "input-engraver.hh"
#include "engraver-group.hh"

bool
Input_engraver::is_name_b(String n)
{
    for (int i=0; i < alias_str_arr_.size(); i++)
	if (alias_str_arr_[i] == n)
	    return true;
    return false;
}

void
Input_engraver::print() const
{
#ifndef NPRINT
    mtor << "type " << type_str_;
    mtor << "Consists of ";
    for (int i=0; i< consists_str_arr_.size(); i++)
	mtor << consists_str_arr_[i] << ',';
    mtor << "contains " ;
    for (iter(contains_igrav_p_list_.top(), i); i.ok(); i++) 
	i->print();
#endif 
}



Input_engraver *
Input_engraver::recursive_find(String nm)
{
    if ( is_name_b( nm) )
	return this;

    Input_engraver * r =0;
    iter(contains_igrav_p_list_.top(), i);
    for (; !r &&i.ok(); i++) {
	if (i->recursive_find(nm))
	    r = i.ptr();
    }

    return r;
}

Input_engraver *
Input_engraver::find_igrav_l(String nm)
{
    for (iter(contains_igrav_p_list_.top(), i); i.ok(); i++)
	if (i->is_name_b( nm))
	    return i;

    return 0;
}


Engraver_group_engraver *
Input_engraver::get_group_engraver_p()
{
    Engraver_group_engraver * grav_p = (Engraver_group_engraver*)
	get_engraver_p(type_str_);

    for (int i=0; i < consists_str_arr_.size(); i++) {
	grav_p->add( get_engraver_p( consists_str_arr_[i]) );
    }
    grav_p -> igrav_l_ = this;
    return grav_p;
}


bool
Input_engraver::accept_req_b()
{
    return ! contains_igrav_p_list_.size();
}

void
Input_engraver::add(Input_engraver *ip)
{
    contains_igrav_p_list_.bottom().add(ip);
}

Input_engraver*
Input_engraver::get_default_igrav_l()
{
    if ( contains_igrav_p_list_.size() )
	return contains_igrav_p_list_.top();
    else
	return 0;
}


Input_engraver_list::Input_engraver_list(Input_engraver_list const &s)
{
    for (PCursor<Input_engraver*> pc(s); pc.ok(); pc++) {
	Input_engraver *q = pc;
	Input_engraver *p=new Input_engraver(*q) ; 
	bottom().add(p);
    }
}
