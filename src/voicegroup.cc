#include "request.hh"
#include "complexwalker.hh"
#include "complexstaff.hh"
#include "voicegroup.hh"
#include "register.hh"

Voice_group_registers::Voice_group_registers(Complex_walker*w_l)
    : //text_reg_(w_l),
      stem_beam_reg_(w_l)
//,      script_reg_(w_l),
      //    stem_reg_(w_l)
{
    walk_l_ = w_l;
}

void
Voice_group_registers::pre_move_processing()
{
    stem_beam_reg_.pre_move_processing();
//    script_reg_.pre_move_processing();
//    text_reg_.pre_move_processing();  
}
void
Voice_group_registers::post_move_processing()
{
    stem_beam_reg_.post_move_processing();
}
bool
Voice_group_registers::try_request(Request*r_l)
{
    bool b = stem_beam_reg_.try_request(r_l);
/*    if (!b)
	b|= beam_reg_.try_request(r_l);
    if (!b)
	b|= script_reg_.try_request(r_l);
    if (!b)
	b|=  text_reg_.try_request(r_l);
	*/
    return b;
}
    
void
Voice_group_registers::announce_element(Staff_elem_info i)
{
/*
  if (i.group_regs_l_!= this)
	return;
	*/
    stem_beam_reg_.acknowledge_element(i);
//    text_reg_.announce_element(i);
    //  script_reg_.announce_element(i);
}


void
Voice_group_registers::process_requests()
{
    stem_beam_reg_.process_request();
}

