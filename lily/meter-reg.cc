/*
  meter-reg.cc -- implement Meter_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "meter-reg.hh"
#include "meter.hh"
#include "command-request.hh"

Meter_register::Meter_register()
{
    post_move_processing();
}

bool
Meter_register::try_request(Request*r_l)
{
    Command_req* creq_l= r_l->command();
     if (!creq_l || !creq_l->meterchange()) 
	return false;
     Meter_change_req *m = creq_l->meterchange();
     if (meter_req_l_ && meter_req_l_->compare(*m))
	 return false;
     
    meter_req_l_ = m;
    return true;
}

void
Meter_register::process_requests()
{
    if (meter_req_l_ ) {
	Array<Scalar> args;
	args.push(meter_req_l_->beats_i_);
	args.push(meter_req_l_->one_beat_i_);
	
	meter_p_ = new Meter(args);
    }

    if (meter_p_)
	announce_element(Staff_elem_info(meter_p_, meter_req_l_) );
}

void
Meter_register::pre_move_processing()
{
    if (meter_p_) {
	Meter * post_p =new Meter(*meter_p_);
	Meter * pre_p =new Meter(*meter_p_);
	
	typeset_breakable_item(pre_p, meter_p_, post_p);
	meter_p_ =0;
    }
}

void
Meter_register::post_move_processing()
{
    meter_req_l_ = 0;
    meter_p_ =0;
}
IMPLEMENT_STATIC_NAME(Meter_register);
