#include "voice.hh"
#include "request.hh"
#include "register.hh"
#include "notehead.hh"
#include "complexwalker.hh"
#include "localkeyitem.hh"

Staff_elem_info::Staff_elem_info(Staff_elem*s_l, Request*r_l,
				 Request_register *reg_l)
{
    elem_p_ = s_l;
    voice_l_ = r_l->elt_l_->voice_l_;
    req_l_ = r_l;
    group_regs_l_ = 0;
    origin_reg_l_ = reg_l;
}
Staff_elem_info::Staff_elem_info()
{
    elem_p_ = 0;
    voice_l_ = 0;

    group_regs_l_ = 0;
    origin_reg_l_ = 0;
    req_l_ = 0;
}
/****************/

Request_register::Request_register()
{
    walk_l_=0;
}

Request_register::Request_register(Complex_walker*w_l)
{
    walk_l_=w_l;    
}

void
Request_register::pre_move_processing()
{    
    do_pre_move_process();
    accepted_req_arr_.set_size(0);
}
void
Request_register::post_move_processing()
{    
    do_post_move_process();
}


/****************/

Local_key_register::Local_key_register(Complex_walker*w)
    : Request_register(w)
{
    key_item_p_ = 0;    
}
bool
Local_key_register::try_request(Request*)

{
    return false;
}

void
Local_key_register::process_request()
{
}
void
Local_key_register::do_pre_move_process()
{
    if (key_item_p_) {
	walk_l_->typeset_element(key_item_p_);
	key_item_p_ = 0;
    }
}
void
Local_key_register::acknowledge_element(Staff_elem_info info)
{    
    if (info.req_l_->melodic()) {
	Melodic_req * melodic_l_ = info.req_l_->melodic();

	if( melodic_l_->forceacc ||
	    walk_l_->local_key_.oct(melodic_l_->octave).acc(melodic_l_->notename)
	    != melodic_l_->accidental) {
	    Item * support_l_ = info.elem_p_->item();
	

	    if (!key_item_p_) {
		key_item_p_ = new Local_key_item(walk_l_->clef_.c0_pos);
		key_item_p_->c0_position = walk_l_->clef_.c0_pos;
	    }
	    
	    key_item_p_->add(melodic_l_);
	    key_item_p_->add(support_l_);
	    walk_l_->local_key_.oct(melodic_l_->octave)
		.set(melodic_l_->notename, melodic_l_->accidental);
	}
    }
}
