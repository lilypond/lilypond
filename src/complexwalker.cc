#include "associter.hh"
#include "script.hh"
#include "request.hh"
#include "voice.hh"
#include "clef.hh"
#include "pscore.hh"
#include "complexstaff.hh"
#include "complexwalker.hh"
#include "sccol.hh"
#include "debug.hh"
#include "keyitem.hh"
#include "clefitem.hh"
#include "voicegroup.hh"
#include "register.hh"

Rhythmic_grouping
cparse_grouping(Array<Scalar> a, Moment one_beat)
{
    Array<int> r;
    for (int i= 0 ; i < a.size(); i++)
	r.push(a[i]);
    Moment here =0.0;

    Array<Rhythmic_grouping*> children;
    for (int i=0; i < r.size(); i++) {
	
	Moment last = here;
	here += one_beat * r[i];
	children.push(
	    new Rhythmic_grouping(MInterval(last, here), r[i] )
	    );
    }
    return Rhythmic_grouping(children);
}

void
Complex_walker::do_INTERPRET_command(Command*com)
{
    Array<Scalar> args(com->args);
    args.del(0);
    if (com->args[0] == "GROUPING") {	
	default_grouping = cparse_grouping(args,
					  col()->tdescription_->one_beat);
    }else if (com->args[0] == "NEWMEASURE") {
	local_key_.reset(key_);

    } else if (com->args[0] == "KEY") {
	
	if (col()->when() > Moment(0)) {
	    assert(!oldkey_undo);
	    oldkey_undo = new Array<int>( key_.oldkey_undo(args));
	}
	
	typesetkey = key_.read(args);
	local_key_.reset(key_);	
    } else if (com->args[0] == "CLEF") {
	clef_.read(args);
    } else {
	WARN << " ignoring INTERPRET command: " << com->args[0]<< "\n";
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
    announce_info_arr_.push(info);
}

void
Complex_walker::do_announces()
{

    for (int i = 0; i < announce_info_arr_.size(); i++){
	Staff_elem_info info = announce_info_arr_[i];
	for (iter_top(voice_reg_list_,j); j.ok(); j++) {
	    j->announce_element(info);
	}
	group_regs_.announce_element(info);
	local_key_reg_.acknowledge_element(info);
    }
}

Voice_registers *
Complex_walker::find_voice_reg(Voice*v_l)
{
    for (iter_top(voice_reg_list_, i); i.ok(); i++) {
	if (i->voice_l_ == v_l)
	    return i;
    }
    
    Voice_registers *regs_p=new Voice_registers(this,v_l);
    voice_reg_list_.bottom().add (regs_p);
    //voice_reg_map_[v_l] = regs_p;
    return regs_p;
}

void
Complex_walker::try_request(Request*req)
{
    bool b;
    if (req->note() || req->rest()|| req->slur()) {
	Voice *v_l = req->elt_l_->voice_l_;
	Voice_registers *vregs_l = find_voice_reg(v_l);
	
	b = vregs_l->try_request(req);
	    
    } else {
	b = group_regs_.try_request(req);
	if (!b)
	    b = local_key_reg_.try_request(req);
    }
    if (!b)
	WARN<< "junking request: " <<req->name() <<"\n";
}

void
Complex_walker::process_requests()
{
    Complex_column*c = col();
//    Complex_staff *s = staff();

    for (int i=0; i < c->todo_l_arr_.size(); i++) {
	try_request(c->todo_l_arr_[i]);
    }

    regs_process_requests();
    do_announces();
}

void
Complex_walker::regs_process_requests()
{
    for (iter_top(voice_reg_list_,j); j.ok(); j++) {
	j->process_requests();
    }
    group_regs_.process_requests();
    local_key_reg_.process_request();
}

void
Complex_walker::typeset_element(Staff_elem *elem_p)
{
    if (elem_p->spanner())
	pscore_l_->typeset_spanner(elem_p->spanner(), staff()->theline_l_);
    else
	col()->typeset_item(elem_p->item()); 
}

Complex_walker::Complex_walker(Complex_staff*s)
    : Staff_walker(s, s->theline_l_->pscore_l_),
      group_regs_(this),
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
    group_regs_.pre_move_processing();
    local_key_reg_.pre_move_processing();
}

void
Complex_walker::do_post_move()
{
    processed_clef =false;    
    processed_key = false;
    processed_bar_priority = 0;
    
    for (iter_top(voice_reg_list_,i); i.ok(); i++) {
	i->post_move_processing();   
    }
    announce_info_arr_.set_size(0);
    group_regs_.post_move_processing();
    local_key_reg_.post_move_processing();
}
