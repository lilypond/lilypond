#include "musicalrequest.hh"
#include "voicegroup.hh"
#include "register.hh"
#include "slurreg.hh"
#include "headreg.hh"

Voice_registers::Voice_registers(Complex_walker*c_l, Voice *v_p)
{
    voice_l_ = v_p;
    head_reg_ = new Notehead_register(c_l);
    slur_reg_ = new Slur_register(c_l);
}
Voice_registers::~Voice_registers()
{
    delete head_reg_;
    delete slur_reg_;
}
bool
Voice_registers::try_request(Request * r_l)
{
    bool b = head_reg_->try_request(r_l);
    if (!b)
	b = slur_reg_->try_request(r_l);
    return b;
}

void
Voice_registers::acknowledge_element(Staff_elem_info i)
{
    if (i.voice_l_ != voice_l_)
	return;
    if (i.origin_reg_l_ != slur_reg_)
	slur_reg_->acknowledge_element(i);
}

void
Voice_registers::pre_move_processing()
{
    head_reg_->pre_move_processing();
    slur_reg_->pre_move_processing();
}
void
Voice_registers::post_move_processing()
{
    head_reg_->post_move_processing();
    slur_reg_->post_move_processing();
}

void
Voice_registers::process_requests()
{
    head_reg_->process_request();
    slur_reg_->process_request();
}

bool
Voice_registers::acceptable_request(Request*r)
{
    return (r->rest() || r->note() || r->slur());
    
}

void
Voice_registers::set_dir(int i)
{
    head_reg_->set_dir(i);
    slur_reg_->set_dir(i);
}
