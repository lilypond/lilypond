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
    bool b = (n == name_str_);
    for (int i=0; !b && i < alias_str_arr_.size(); i++)
	b = b || (alias_str_arr_[i] == n);
    return b;
}

void
Input_engraver::print() const
{
#ifndef NPRINT
    mtor << "name " << name_str_;
    mtor << "Consists of ";
    for (int i=0; i< consists_str_arr_.size(); i++)
	mtor << consists_str_arr_[i] << ',';
    mtor << "contains " ;
    for (iter(contains_igrav_p_list_.top(), i); i.ok(); i++) 
	i->print();
#endif 
}

/*
  UGH. Global.
 */
Link_array<Input_engraver> igravs_p_arr;

void
add_global_input_engraver(Input_engraver *grav_p)
{
    igravs_p_arr.push(grav_p);
}

Input_engraver *
lookup_grav(String nm)
{
    for (int i=0; i < igravs_p_arr.size(); i++)
	if (igravs_p_arr[i]->is_name_b(nm))
	    return igravs_p_arr[i];

    error("can't find reg `" + nm + "'");
}



Input_engraver *
Input_engraver::recursive_find(String nm)
{
    if ( is_name_b( nm) )
	return this;

    Input_engraver * r =0;
    for (iter(contains_igrav_p_list_.top(), i); !r &&i.ok(); i++)
	r = i->recursive_find(nm);

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
	get_engraver_p(name_str_);

    
     
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
    return contains_igrav_p_list_.top();
}
