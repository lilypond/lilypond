/*
  meter-reg.cc -- implement Meter_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "meter-grav.hh"
#include "meter.hh"
#include "command-request.hh"
#include "score-grav.hh"

Meter_engraver::Meter_engraver()
{ 
    meter_req_l_ = 0;
    meter_p_ =0;
    default_grouping_ = Rhythmic_grouping(MInterval(0,4),4); // ugh
}

void
Meter_engraver::fill_staff_info(Staff_info&inf)
{
    inf.time_C_ = &time_;
    inf.rhythmic_C_ = &default_grouping_;
}

bool
Meter_engraver::do_try_request(Request*r)
{
    bool gotcha = false;

    if (r->command() && r->command()->timing()) {
	gotcha = true;
	Timing_req * tr_l = r->command()->timing();
	Meter_change_req *m_l = tr_l->meterchange();
	if (m_l) {
	    meter_req_l_ = m_l;

	    int b_i= m_l->beats_i_;
	    int o_i = m_l->one_beat_i_;
	    if (! time_.allow_meter_change_b() )
		tr_l->warning("Meter change not allowed here");
	    else{
		time_.set_meter(b_i, o_i);
		default_grouping_ = 
		    Rhythmic_grouping(MInterval(0,Moment(b_i, o_i)), b_i);
	    }
	} else if (tr_l->partial()) {
	    Moment m = tr_l->partial()->duration_;
	    String error = time_.try_set_partial_str(m);
	    if (error != "") {
		tr_l->warning(error);
	    } else 
		time_.setpartial(m);
	} else if (tr_l->barcheck()) {
	    if (time_.whole_in_measure_) {
		tr_l ->warning( "Barcheck failed");
	    
		time_.whole_in_measure_ = 0; // resync
		time_.error_b_ = true;
	    }

	} else if (tr_l->cadenza()) {
	    time_.set_cadenza(tr_l->cadenza()->on_b_);

	} else if (tr_l->measuregrouping()) {
	    default_grouping_ = parse_grouping(
		    tr_l->measuregrouping()->beat_i_arr_,
		    tr_l->measuregrouping()->elt_length_arr_);

	}	
    }
    
    return gotcha;
}

void
Meter_engraver::do_creation_processing()
{
    time_.when_ = get_staff_info().when();
}

void
Meter_engraver::do_process_requests()
{
    if (meter_req_l_ ) {
	Array<Scalar> args;
	args.push(meter_req_l_->beats_i_);
	args.push(meter_req_l_->one_beat_i_);
	
	meter_p_ = new Meter(args);
    }

    if (meter_p_)
	announce_element(Score_elem_info(meter_p_, meter_req_l_) );
}

void
Meter_engraver::do_pre_move_processing()
{
    if (meter_p_) {
	typeset_breakable_item(meter_p_);
	meter_p_ =0;
	meter_req_l_ = 0;
    }

    Engraver_group_engraver * grav_l = daddy_grav_l_;
    while (grav_l->daddy_grav_l_) {
	grav_l = grav_l->daddy_grav_l_;
    }
    
    assert( grav_l->name() == Score_engraver::static_name());
    if (!time_.cadenza_b_)
	((Score_engraver*)grav_l)->add_moment_to_process( time_.next_bar_moment());
}

void
Meter_engraver::do_post_move_processing()
{
    time_.add( get_staff_info().when()  - time_.when_ );
}

IMPLEMENT_STATIC_NAME(Meter_engraver);
ADD_THIS_ENGRAVER(Meter_engraver);
IMPLEMENT_IS_TYPE_B1(Meter_engraver,Request_engraver); 
