#include "request.hh"
#include "complexwalker.hh"
#include "complexstaff.hh"
#include "voicegroup.hh"
#include "register.hh"

static int temp_id_count;

Voice_group_registers::Voice_group_registers(Complex_walker*w_l, String id)
    : text_reg_(w_l),
      stem_beam_reg_(w_l),
      script_reg_(w_l)
{
    walk_l_ = w_l;
    if (id=="")
	id = __FUNCTION__ + String(temp_id_count++);
    group_id_str_ = id;
    dir_i_ = 0;
}

void
Voice_group_registers::pre_move_processing()
{
    stem_beam_reg_.pre_move_processing();
    script_reg_.pre_move_processing();
    text_reg_.pre_move_processing();  
}

void
Voice_group_registers::post_move_processing()
{
    stem_beam_reg_.post_move_processing();
    text_reg_.post_move_processing();
    script_reg_.post_move_processing();
}

bool
Voice_group_registers::try_request(Request*r_l)
{
    if (r_l->groupfeature()) {
	dir_i_ = r_l->groupfeature()->stemdir_i_;
	return true;
    } 
    bool b = stem_beam_reg_.try_request(r_l);
    if (!b)
	b|= script_reg_.try_request(r_l);
    if (!b)
	b|=  text_reg_.try_request(r_l);

    return b;
}
    
void
Voice_group_registers::announce_element(Staff_elem_info i)
{
    if (i.group_regs_l_!= this)
	return;
	
    stem_beam_reg_.acknowledge_element(i);
    script_reg_.acknowledge_element(i);
    text_reg_.acknowledge_element(i);
}


void
Voice_group_registers::process_requests()
{
    stem_beam_reg_.process_request();
    if (dir_i_)
	stem_beam_reg_.set_dir (dir_i_);
    
    script_reg_.process_request();
    text_reg_.process_request();
}


bool
Voice_group_registers::acceptable_request(Request*r)
{
    return (r->stem() || r->beam() || r->text() || r->script() ||
	    r->groupfeature());
}
