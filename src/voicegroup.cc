#include "request.hh"
#include "voicegroup.hh"
#include "register.hh"
#include "textreg.hh"
#include "stembeamreg.hh"
#include "scriptreg.hh"
#include "complexwalker.hh"


static int temp_id_count;

Voice_group_registers::Voice_group_registers(Complex_walker*w_l, String id)
{
    walk_l_ = w_l;
    text_reg_=new Text_register(w_l);
    stem_beam_reg_= new Stem_beam_register(w_l);
    script_reg_ = new Script_register(w_l);
    if (id=="")
	id = __FUNCTION__ + String(temp_id_count++);
    group_id_str_ = id;
    dir_i_ = 0;
}

Voice_group_registers::~Voice_group_registers()
{
    delete text_reg_;
    delete stem_beam_reg_;
    delete script_reg_;
}
void
Voice_group_registers::pre_move_processing()
{
    stem_beam_reg_->pre_move_processing();
    script_reg_->pre_move_processing();
    text_reg_->pre_move_processing();  
}

void
Voice_group_registers::post_move_processing()
{
    stem_beam_reg_->post_move_processing();
    text_reg_->post_move_processing();
    script_reg_->post_move_processing();
}

bool
Voice_group_registers::try_request(Request*r_l)
{
    if (r_l->groupfeature()) {
	set_dir(r_l->groupfeature()->stemdir_i_);
	return true;
    } 
    bool b = stem_beam_reg_->try_request(r_l);
    if (!b)
	b|= script_reg_->try_request(r_l);
    if (!b)
	b|=  text_reg_->try_request(r_l);

    return b;
}
    
void
Voice_group_registers::acknowledge_element(Staff_elem_info i)
{
    if (i.group_regs_l_!= this)
	return;
	
    stem_beam_reg_->acknowledge_element(i);
    script_reg_->acknowledge_element(i);
    text_reg_->acknowledge_element(i);
}

void
Voice_group_registers::set_dir(int i)
{
    stem_beam_reg_->set_dir(i);
    script_reg_->set_dir(i);
    text_reg_->set_dir(i);
    
    Array<Voice_registers*> vr_arr (walk_l_->get_voice_regs(this));
    for (int j=0; j<vr_arr.size(); j++) {
	if (vr_arr[j])
	    vr_arr[j]->set_dir(i);
}
}
void
Voice_group_registers::process_requests()
{
    stem_beam_reg_->process_request();    
    script_reg_->process_request();
    text_reg_->process_request();
}


bool
Voice_group_registers::acceptable_request(Request*r)
{
    return (r->stem() || r->beam() || r->text() || r->script() ||
	    r->groupfeature());
}
