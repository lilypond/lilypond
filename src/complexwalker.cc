/*
  complexwalker.cc -- implement Complex_walker

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "associter.hh"
#include "script.hh"
#include "musicalrequest.hh"
#include "voice.hh"
#include "pscore.hh"
#include "complexstaff.hh"
#include "debug.hh"
#include "voicegroup.hh"
#include "complexwalker.hh"
#include "misc.hh"
#include "commandrequest.hh"
#include "clefreg.hh"
#include "localkeyreg.hh"
#include "keyreg.hh"
#include "meterreg.hh"
#include "barreg.hh"


void
Complex_walker::announce_element(Staff_elem_info info)
{
    info.group_regs_l_ = find_voice_group((Voice*)info.voice_l_);
    announce_info_arr_.push(info);
}

void
Complex_walker::do_announces()
{
    Request dummy_req;
    for (int i = 0; i < announce_info_arr_.size(); i++){
	Staff_elem_info info = announce_info_arr_[i];
	if (!info.req_l_)
	    info.req_l_ = &dummy_req;
	
	for (iter_top(voice_reg_list_,j); j.ok(); j++) {
	    j->acknowledge_element(info);
	}
	for (iter_top (	group_reg_list_, j); j.ok(); j++) {
	    j->acknowledge_element(info);
	}
	bar_reg_p_->acknowledge_element(info);
	clef_reg_p_->acknowledge_element(info);
	key_reg_p_->acknowledge_element(info);
	meter_reg_p_->acknowledge_element(info);
	
	local_key_reg_p_->acknowledge_element(info);
    }
}

Voice_registers *
Complex_walker::find_voice_reg(Voice*v_l)const
{
   for (iter_top(voice_reg_list_, i); i.ok(); i++) {
	if (i->voice_l_ == v_l)
	    return i;
   }
   return 0;
}

Voice_registers*
Complex_walker::get_voice_reg(Voice*v_l)
{
    Voice_registers *regs_p=find_voice_reg(v_l);
    if (regs_p)
	return regs_p;
    
    regs_p = new Voice_registers(this,v_l);
    voice_reg_list_.bottom().add (regs_p);
    return regs_p;
}

Voice_group_registers *
Complex_walker::find_voice_group(Voice* v_l)const
{
    if (!voice_group_map_.elt_query(v_l))
	return 0;
    else return voice_group_map_[v_l];
}

Voice_group_registers *
Complex_walker::find_voice_group(const char *id)const
{
    for (iter_top(group_reg_list_, i); i.ok(); i++)
	if (i->group_id_str_ == id)
	    return i;
    return 0;
}


Voice_group_registers *
Complex_walker::get_voice_group(Voice *v_l)
{
    Voice_group_registers *group_p = find_voice_group(v_l);
    if (group_p)
	return group_p;
    
    group_p = new Voice_group_registers(this);
    group_reg_list_.bottom().add(group_p);
    voice_group_map_[v_l] = group_p;
    return group_p;
}


Voice_group_registers *
Complex_walker::get_voice_group(const char* id)
{
    Voice_group_registers *group_p = find_voice_group(id);
    if (group_p)
	return group_p;
    group_p = new Voice_group_registers(this,id);
    group_reg_list_.bottom().add(group_p);
    return group_p;
}

void 
Complex_walker::do_change_group(Voice * v, String group_id_str)
{
    voice_group_map_[v] = get_voice_group(group_id_str);
}

bool
Complex_walker::try_command_request(Nonmusical_req *req_l)
{
    bool b=false;
    Voice *voice_l = (Voice*)req_l->elt_l_->voice_l_; // ugh
    if (req_l->groupchange()){
	do_change_group(voice_l, req_l->groupchange()->newgroup_str_);
	b|= true;
    } else if(req_l->groupfeature()) {
	Voice_group_registers* reg_l = get_voice_group(voice_l);
	b |= reg_l->try_request(req_l);
    } else {
	if (!b)
	    b |= bar_reg_p_->try_request(req_l);
	b |= clef_reg_p_->try_request(req_l);
	if (!b) 
	    b |= key_reg_p_->try_request(req_l);
	if (!b)
	    b |= meter_reg_p_->try_request(req_l);
    }
    return b;
}

void
Complex_walker::try_request(Request*req)
{
    bool b=false;
    Voice *voice_l = (Voice*)req->elt_l_->voice_l_; // ahh. This sux

    if (req->nonmus()) {
	b = try_command_request(req->nonmus());
    } else if (Voice_registers::acceptable_request(req)) {
	Voice_registers *vregs_l = get_voice_reg(voice_l);
	b = vregs_l->try_request(req);
    } else if (Voice_group_registers::acceptable_request(req)){
	Voice_group_registers* reg_l = get_voice_group(voice_l);
	b = reg_l->try_request(req);
    } 

    if (!b)
	warning("junking request: "  + String(req->name()),
		req->defined_ch_c_l_);
}

void
Complex_walker::process_requests()
{
    Staff_column*c =ptr();

    for (int i=0; i < c->commandreq_l_arr_.size(); i++) {
	try_request(c->commandreq_l_arr_[i]);
    }

    for (int i=0; i < c->musicalreq_l_arr_.size(); i++) {
	try_request(c->musicalreq_l_arr_[i]);
    }

    regs_process_requests();
    do_announces();
}

void
Complex_walker::regs_process_requests()
{
    for (iter_top(voice_reg_list_, j); j.ok(); j++) {
	j->process_requests();
    }
    for (iter_top(group_reg_list_, j); j.ok(); j++) 
	j->process_requests();
    
    
    bar_reg_p_->process_request();
    clef_reg_p_->process_request();
    key_reg_p_->process_request();
    meter_reg_p_->process_request();
    local_key_reg_p_->process_request();
}

void
Complex_walker::typeset_element(Staff_elem *elem_p)
{
    if (!elem_p)
	return;
    if (elem_p->spanner())
	pscore_l_->typeset_spanner(elem_p->spanner(), staff()->pstaff_l_);
    else
	ptr()->typeset_musical_item(elem_p->item()); 
}

Complex_walker::Complex_walker(Complex_staff*s)
    : Staff_walker(s, s->pstaff_l_->pscore_l_)
{
    local_key_reg_p_= new Local_key_register(this);
    clef_reg_p_= new Clef_register(this);
    key_reg_p_ = new Key_register(this);
    bar_reg_p_ = new Bar_register(this);
    meter_reg_p_ = new Meter_register(this);
    
    do_post_move();
}


Complex_walker::~Complex_walker()
{
    delete local_key_reg_p_;
    delete clef_reg_p_;
    delete meter_reg_p_;
    delete bar_reg_p_;
    delete key_reg_p_;
}

int
Complex_walker::c0_position_i()const
{
    return c0_position_i_;
}

void
Complex_walker::set_c0_position(int p)
{
    c0_position_i_ =p;
}

Complex_staff*
Complex_walker::staff()
{
    return (Complex_staff*) staff_l_;
}


void
Complex_walker::do_pre_move()
{
    for (iter_top(voice_reg_list_,i); i.ok(); i++) {
	i->pre_move_processing();   
    }
    for (iter_top (group_reg_list_, j); j.ok(); j++) 
	j->pre_move_processing();
    local_key_reg_p_->pre_move_processing();
    bar_reg_p_->pre_move_processing();
    clef_reg_p_->pre_move_processing();
    key_reg_p_->pre_move_processing();
    meter_reg_p_->pre_move_processing();

    ptr()->typeset_breakable_items(prebreak_item_p_arr_,
				   nobreak_item_p_arr_,
				   postbreak_item_p_arr_);
}

void
Complex_walker::do_post_move()
{
    for (iter_top(voice_reg_list_,i); i.ok(); i++) {
	i->post_move_processing();   
    }
    announce_info_arr_.set_size(0);
    for (iter_top (group_reg_list_, j); j.ok(); j++) 
	j->post_move_processing();
    
    local_key_reg_p_->post_move_processing();
    bar_reg_p_->post_move_processing();
    clef_reg_p_->post_move_processing();
    key_reg_p_->post_move_processing();
    meter_reg_p_->post_move_processing();

}

Array<Voice_registers*>
Complex_walker::get_voice_regs(Voice_group_registers* group_regs_l) const
    return l_arr;
{
    for (Assoc_iter<Voice*,Voice_group_registers*> i(voice_group_map_);
	 i.ok(); i++) {
	if (i.val() == group_regs_l)
	    l_arr.push(find_voice_reg(i.key()));
    }
}

void
Complex_walker::typeset_breakable_item(Item * pre_p , Item * nobreak_p,
				       Item * post_p)
{
    if (pre_p)
	prebreak_item_p_arr_.push(pre_p);
    if (nobreak_p)
	nobreak_item_p_arr_.push(nobreak_p);
    if (post_p)
	postbreak_item_p_arr_.push(post_p);
}
