#include "associter.hh"
#include "script.hh"
#include "request.hh"
#include "voice.hh"
#include "clef.hh"
#include "pscore.hh"
#include "complexstaff.hh"
#include "debug.hh"
#include "keyitem.hh"
#include "clefitem.hh"
#include "voicegroup.hh"
#include "localkeyreg.hh"
#include "complexwalker.hh"
#include "misc.hh"

Rhythmic_grouping
parse_grouping(Array<Scalar> const &a)
{
    Array<int> r;
    Array<Moment> grouplen_arr;
    for (int i= 0 ; i < a.size()/2; ) {
	r.push(a[i++]);
	grouplen_arr.push(Moment(1,(int) a[i++]));
    }
    Moment here =0;

    Array<Rhythmic_grouping*> children;
    for (int i=0; i < r.size(); i++) {
	
	Moment last = here;
	here += grouplen_arr[i] * Rational(r[i]);
	children.push(
	    new Rhythmic_grouping(MInterval(last, here), r[i] ));
    }
    return Rhythmic_grouping(children);
}

void
Complex_walker::do_INTERPRET_command(Command*com_l)
{
    Array<Scalar> args(com_l->args);
    args.del(0);
    String com = com_l->args[0];
    
    if (com == "GROUPING") {	
	default_grouping = parse_grouping(args);
    }else if (com == "NEWMEASURE") {
	local_key_.reset(key_);

    } else if (com == "KEY") {
	
	if (col()->when() > Moment(0)) {
	    assert(!oldkey_undo);
	    oldkey_undo = new Array<int>( key_.oldkey_undo(args));
	}
	
	typesetkey = key_.read(args);
	local_key_.reset(key_);	
    } else if (com == "CLEF") {
	clef_.set_type(args[0]);
    } else {
	WARN << " ignoring INTERPRET command: " << com<< "\n";
    }
}

void
Complex_walker::do_TYPESET_command(Command*com)
{
    /* ignore these commands if non-default versions have been
      processed.  */
    if (com->args[0] == "CURRENTKEY") 
	if (processed_key) 
	    return;
	else
	    com->args[0] = "KEY"; // urgh
    
    if (com->args[0] == "CURRENTCLEF") {
	if (processed_clef) 
	    return;
    }
    if (com->args[0] == "BAR") {
	
	if (processed_bar_priority > com->priority)
	    return;
	else
	    processed_bar_priority = com->priority;
    }

    Item* i = staff()->get_TYPESET_item(com);
    if (!i)
	return;

    if (com->args[0] == "KEY") {
	((Keyitem*) i)->read(clef_);
	if (oldkey_undo) {
	    ((Keyitem*) i)->read(*oldkey_undo);
	    delete oldkey_undo;
	    oldkey_undo = 0;
	}
	processed_key = true;
	
	((Keyitem*) i)->read(typesetkey); // ugh	
    }

    if (com->args[0] == "CLEF"||com->args[0] == "CURRENTCLEF") {
	processed_clef =true;
	Clef_item*c=(Clef_item*)i;
	c->read(clef_);
	c->change = (break_status != BREAK_POST - BREAK_PRE);
    }
    col()->typeset_item_directional(i, 1, break_status);
}

void
Complex_walker::announce_element(Staff_elem_info info)
{
    info.group_regs_l_ = find_voice_group((Voice*)info.voice_l_);
    announce_info_arr_.push(info);
}

void
Complex_walker::do_announces()
{
    for (int i = 0; i < announce_info_arr_.size(); i++){
	Staff_elem_info info = announce_info_arr_[i];
	for (iter_top(voice_reg_list_,j); j.ok(); j++) {
	    j->acknowledge_element(info);
	}
	for (iter_top (	group_reg_list_, j); j.ok(); j++) {
	    j->acknowledge_element(info);
	}
	local_key_reg_.acknowledge_element(info);
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

void
Complex_walker::try_request(Request*req)
{
    bool b=false;
    Voice *voice_l = req->elt_l_->voice_l_;

    if (req->groupchange()) {
	do_change_group(voice_l, req->groupchange()->newgroup_str_);
	b = true;
    } else if (Voice_registers::acceptable_request(req)) {
	Voice_registers *vregs_l = get_voice_reg(voice_l);
	b = vregs_l->try_request(req);
    } else if (Voice_group_registers::acceptable_request(req)){
	Voice_group_registers* reg_l = get_voice_group(voice_l);
	b = reg_l->try_request(req);
    } 

    if (!b)
	warning("junking request: "  + String(req->name()), req->defined_ch_c_l_m);
}

void
Complex_walker::process_requests()
{
    Complex_column*c = col();

    for (int i=0; i < c->first_l_arr_.size(); i++) {
	try_request(c->first_l_arr_[i]);
    }

    for (int i=0; i < c->second_l_arr_.size(); i++) {
	try_request(c->second_l_arr_[i]);
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
    
    local_key_reg_.process_request();
}

void
Complex_walker::typeset_element(Staff_elem *elem_p)
{
    if (!elem_p)
	return;
    if (elem_p->spanner())
	pscore_l_->typeset_spanner(elem_p->spanner(), staff()->pstaff_l_);
    else
	col()->typeset_item(elem_p->item()); 
}

Complex_walker::Complex_walker(Complex_staff*s)
    : Staff_walker(s, s->pstaff_l_->pscore_l_),
      local_key_reg_(this)      
{
    oldkey_undo = 0;
    do_post_move();
}

Complex_staff*
Complex_walker::staff()
{
    return (Complex_staff*) staff_l_;
}

Complex_column*
Complex_walker::col()
{
    return (Complex_column*) *(*this);
}

void
Complex_walker::do_pre_move()
{
    for (iter_top(voice_reg_list_,i); i.ok(); i++) {
	i->pre_move_processing();   
    }
    for (iter_top (group_reg_list_, j); j.ok(); j++) 
	j->pre_move_processing();
    local_key_reg_.pre_move_processing();
}

void
Complex_walker::do_post_move()
{
    processed_clef = false;    
    processed_key = false;
    processed_bar_priority = 0;
    
    for (iter_top(voice_reg_list_,i); i.ok(); i++) {
	i->post_move_processing();   
    }
    announce_info_arr_.set_size(0);
    for (iter_top (group_reg_list_, j); j.ok(); j++) 
	j->post_move_processing();
    local_key_reg_.post_move_processing();
}

Array<Voice_registers*>
Complex_walker::get_voice_regs(Voice_group_registers* group_regs_l) const
    return l_arr;
{
    for (Assoc_iter<Voice*,Voice_group_registers*> i(voice_group_map_); i.ok(); i++) {
	if (i.val() == group_regs_l)
	    l_arr.push(find_voice_reg(i.key()));
    }
}
