/*
  input-translator.cc -- implement Input_translator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

//#include "translator-admin.hh"
#include "debug.hh"
#include "engraver.hh"
#include "input-translator.hh"
#include "parray.hh"
#include "input-translator.hh"
#include "engraver-group.hh"

bool
Input_translator::is_name_b(String n)
{
    for (int i=0; i < alias_str_arr_.size(); i++)
	if (alias_str_arr_[i] == n)
	    return true;
    return false;
}

void
Input_translator::print() const
{
#ifndef NPRINT
    mtor << base_str_ <<" " << type_str_<<" {\n";
    mtor << "Consists of ";
    for (int i=0; i< consists_str_arr_.size(); i++)
	mtor << consists_str_arr_[i] << ',';
    if (contains_itrans_p_list_.size())
	mtor << "\nContains " ;
    for (iter(contains_itrans_p_list_.top(), i); i.ok(); i++) 
	i->print();
    mtor << "}\n";
#endif 
}



Input_translator *
Input_translator::recursive_find(String nm)
{
    if ( is_name_b( nm) )
	return this;

    Input_translator * r =0;
    iter(contains_itrans_p_list_.top(), i);
    for (; !r &&i.ok(); i++) {
	if (i->recursive_find(nm))
	    r = i.ptr();
    }

    return r;
}

Input_translator *
Input_translator::find_itrans_l(String nm)
{
    for (iter(contains_itrans_p_list_.top(), i); i.ok(); i++)
	if (i->is_name_b( nm))
	    return i;

    return 0;
}


Engraver_group_engraver *
Input_translator::get_group_engraver_p()
{
    assert (base_str_ == "Engraver");
    Engraver_group_engraver * grav_p = (Engraver_group_engraver*)
	get_engraver_p(type_str_);

    for (int i=0; i < consists_str_arr_.size(); i++) {
	grav_p->add( get_engraver_p( consists_str_arr_[i]) );
    }
    grav_p->itrans_l_ = this;
    grav_p->id_str_ = default_id_str_;
    return grav_p;
}

#if 0
Performer_group_performer*
Input_translator::get_group_performer_p()
{    
    assert (base_str_ == "Performer");
    Performer_group_performer * grav_p = (Performer_group_performer*)
	get_performer_p(type_str_);

    for (int i=0; i < consists_str_arr_.size(); i++) {
	grav_p->add( get_performer_p( consists_str_arr_[i]) );
    }
    grav_p->itrans_l_ = this;
    grav_p->id_str_ = default_id_str_;
    return grav_p;
}
#endif    

bool
Input_translator::accept_req_b()
{
    return ! contains_itrans_p_list_.size();
}

void
Input_translator::add(Input_translator *ip)
{
    contains_itrans_p_list_.bottom().add(ip);
}

Input_translator*
Input_translator::get_default_itrans_l()
{
    if ( contains_itrans_p_list_.size() )
	return contains_itrans_p_list_.top();
    else
	return 0;
}


Input_translator_list::Input_translator_list(Input_translator_list const &s)
{
    for (PCursor<Input_translator*> pc(s); pc.ok(); pc++) {
	Input_translator *q = pc;
	Input_translator *p=new Input_translator(*q) ; 
	bottom().add(p);
    }
}
