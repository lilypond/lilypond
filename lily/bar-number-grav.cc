/*
  bar-number-grav.cc -- implement Bar_number_grav

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "bar-number-grav.hh"
#include "script.hh"
#include "text-def.hh"
#include "command-request.hh"
#include "bar.hh"

Bar_number_grav::Bar_number_grav()
{
    number_i_ =1;
    script_p_ =0;
}

static Command_script_req dummy;

void
Bar_number_grav::acknowledge_element (Score_elem_info i)
{
    if (i.origin_grav_l_arr_.size() == 1 &&
	i.elem_l_->is_type_b (Bar::static_name()) && !script_p_) {

	script_p_ = new Script;
	Text_def *td_p =new Text_def;
	td_p->text_str_ = number_i_++;
	script_p_->specs_l_ = td_p;
	script_p_->breakable_b_ = true;
	script_p_->dir_i_ = 1;

	announce_element (Score_elem_info (script_p_, &dummy));
    }
}

void
Bar_number_grav::do_pre_move_processing()
{
    if ( script_p_) {
	typeset_element (script_p_);
	script_p_ =0;
    }
}

IMPLEMENT_IS_TYPE_B1(Bar_number_grav,Engraver);
ADD_THIS_ENGRAVER(Bar_number_grav);
