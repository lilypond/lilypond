/*
  registergroup.cc -- implement Register_group_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "register-group.hh"
#include "register.hh"
#include "debug.hh"
#include "p-score.hh"
#include "score-elem.hh"
#include "input-register.hh"


Register_group_register::Register_group_register()
{
    ireg_l_ =0;
}

void
Register_group_register::set_feature(Feature d)
{
    iter_top(reg_list_, i);
    while (i.ok()) {
	// this construction to ensure clean deletion
	Request_register *reg_l = i++; 
	reg_l->set_feature(d);
    }
}

void
Register_group_register::sync_features()
{
    iter_top(reg_list_, i);
    while (i.ok()) {

	Request_register *reg_l = i++; 
	reg_l->sync_features();
    }
}

void
Register_group_register::do_pre_move_processing()
{
    iter_top(reg_list_, i);
    while (i.ok()) {
	
	Request_register *reg_l = i++; 
	reg_l->pre_move_processing();
    }
}

void
Register_group_register::do_process_requests()
{
    iter_top(reg_list_, i);
    while (i.ok()) {
	
	Request_register *reg_l = i++; 
	reg_l->process_requests();
    }
}


void
Register_group_register::do_post_move_processing()
{
    iter_top(reg_list_, i);
    while (i.ok()) {
		// this construction to ensure clean deletion
	Request_register *reg_l = i++; 
	reg_l->post_move_processing();
    }
}


bool
Register_group_register::contains_b(Request_register* reg_l)const
{
    bool parent_b = Request_register::contains_b(reg_l);
    
    if (parent_b)
	return true;
    for (iter_top(reg_list_, j); j.ok(); j++)
	if (j->contains_b(reg_l))
	    return true;
    return false;
}
	


bool
Register_group_register::do_try_request(Request*req_l)
{
    bool hebbes_b =false;
    for (int i =0; !hebbes_b && i < nongroup_l_arr_.size() ; i++)
	hebbes_b =nongroup_l_arr_[i]->try_request(req_l);
    if (!hebbes_b)
	hebbes_b = daddy_reg_l_->try_request(req_l);
    return hebbes_b ;
}

void
Register_group_register::add(Request_register *reg_p)
{
    reg_list_.bottom().add(reg_p);
    reg_p->daddy_reg_l_ = this;

    if (reg_p->is_type_b(Register_group_register::static_name())) {
	group_l_arr_.push((Register_group_register*)reg_p);
    } else {
	nongroup_l_arr_ .push(reg_p);
    }
}


Request_register *
Register_group_register::remove_register_p(Request_register*reg_l)
{
    group_l_arr_.substitute((Register_group_register*)reg_l,0);
    nongroup_l_arr_.substitute(reg_l,0);
    iterator(reg_list_) reg_cur= reg_list_.find(reg_l);
    
    return reg_cur.remove_p();
}

void
Register_group_register::terminate_register(Request_register*r_l)
{
    Request_register * reg_p =remove_register_p(r_l);
    reg_p->do_removal_processing();
    delete reg_p;
}

IMPLEMENT_IS_TYPE_B1(Register_group_register,Request_register);
IMPLEMENT_STATIC_NAME(Register_group_register);
ADD_THIS_REGISTER(Register_group_register);

void
Register_group_register::do_print()const
{
#ifndef NPRINT
    mtor << "ID: " << id_str_ << "\n";
    for (iter_top(reg_list_, i); i.ok(); i++) 
	i->print();
#endif
}


Register_group_register*
Register_group_register::find_register_l(String n, String id)
{
    if (name() == n && id_str_ == id)
	return this;
    Register_group_register * r = 0;
    for (int i =0; !r && i<  group_l_arr_.size(); i++) {
	r = group_l_arr_[i]->find_register_l(n,id);
    }
    
    return r;
}

Register_group_register*
Register_group_register::find_get_reg_l(String n,String id)
{
    Register_group_register * ret=0;
    if (ireg_l_-> find_ireg_l( n )) {
	ret = find_register_l(n,id);
	if (!ret) {
	    ret = ireg_l_-> find_ireg_l(n) -> get_group_register_p();
	    add (  ret );
	    ret ->id_str_ = id;
	}
    } else if (daddy_reg_l_)
	ret =daddy_reg_l_->find_get_reg_l(n,id);
    else {
	warning("Can't find or create `" + n + "' called `" + id + "'\n");
	ret =0;
    }
    return ret;
}

int
Register_group_register::depth_i()const
{
    return daddy_reg_l_->depth_i()  + 1;
}

Register_group_register*
Register_group_register::ancestor_l(int l)
{
    if (!l || !daddy_reg_l_)
	return this;
    
    return daddy_reg_l_->ancestor_l(l-1);
}

void
Register_group_register::announce_element(Score_elem_info info)
{
    announce_info_arr_.push(info);
    Request_register::announce_element(info);
}

void
Register_group_register::do_announces()
{
    for (int i=0; i < group_l_arr_.size(); i++) {
	group_l_arr_[i]->do_announces();
    }
    
    Request dummy_req;
 
    for (int j =0; j < announce_info_arr_.size(); j++){
       Score_elem_info info = announce_info_arr_[j];
       mtor << "Announcing " << info.elem_l_->name()<<"\n";
       
       if (!info.req_l_)
	    info.req_l_ = &dummy_req;
       for (int i=0; i < nongroup_l_arr_.size(); i++) {
	   if (nongroup_l_arr_[i] != info.origin_reg_l_arr_[0])
	       nongroup_l_arr_[i]->acknowledge_element(info);
       }
    }
    announce_info_arr_.set_size(0);
}


void
Register_group_register::do_removal_processing()
{
    for (iter( reg_list_.top(), i); i.ok(); i++)
	i->do_removal_processing();
}

Staff_info
Register_group_register::get_staff_info()const
{
    Staff_info inf = Request_register::get_staff_info();

    for (int i=0; i < nongroup_l_arr_.size(); i++)
	nongroup_l_arr_[i]->fill_staff_info(inf);
    
    return inf;
}

Register_group_register*
Register_group_register::get_default_interpreter()
{
    if ( interpreter_l() )
	return daddy_reg_l_->get_default_interpreter();

    Register_group_register *reg_p= ireg_l_->
	get_default_ireg_l()->get_group_register_p();
    add(reg_p );
    if (reg_p->interpreter_l())
	return reg_p;
    else
	return reg_p->get_default_interpreter();
}
