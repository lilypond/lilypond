/*
  meterreg.cc -- implement Meter_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "meterreg.hh"
#include "meter.hh"
#include "commandrequest.hh"

Meter_register::Meter_register(Complex_walker*w)
    : Request_register(w)
{
    do_post_move_process();
}

bool
Meter_register::try_request(Request*r_l)
{
    if (!r_l->nonmus()->meterchange()) 
	return false;

    assert(!meter_req_l_);
    meter_req_l_ = r_l->nonmus()->meterchange();

    return true;
}

void
Meter_register::process_request()
{
    if (meter_req_l_ ) {
	Array<Scalar> args;
	args.push(meter_req_l_->beats_i_);
	args.push(meter_req_l_->one_beat_i_);
	
	meter_p_ = new Meter(args);
    }

    if (meter_p_)
	announce_element(Staff_elem_info(meter_p_, meter_req_l_, this) );
}

void
Meter_register::do_pre_move_process()
{
    if (meter_p_) {
	Meter * post_p =new Meter(*meter_p_);
	Meter * pre_p =new Meter(*meter_p_);
	
	typeset_breakable_item(pre_p, meter_p_, post_p);
	meter_p_ =0;
    }
}

void
Meter_register::do_post_move_process()
{
    meter_req_l_ = 0;
    meter_p_ =0;
}
