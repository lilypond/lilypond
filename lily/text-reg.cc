/*
  text-reg.cc -- implement Text_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musicalrequest.hh"
#include "text-reg.hh"
#include "text-item.hh"

Text_register::Text_register()
{
    text_p_ = 0;
    set_feature(Features::dir(0));
    post_move_processing();
}

bool
Text_register::try_request(Request*req_l)
{
    if (!req_l->text())
	return false;
    if (text_req_l_ &&
	Text_req::compare(*req_l->text(), *text_req_l_))

	return false;

    text_req_l_ = req_l->text();
    return true;
}

void
Text_register::process_requests()
{
    
    if (text_req_l_) {
	text_p_ = new Text_item(text_req_l_, 10); // UGH
	announce_element(Staff_elem_info(text_p_, text_req_l_));
    }
}
void
Text_register::pre_move_processing()
{
    if (text_p_) {
	text_p_->dir_i_ = dir_i_;
	typeset_element(text_p_);
	text_p_ = 0;
    }
}
void
Text_register::set_feature(Features i)
{
    dir_i_ = i.direction_i_;
}
void
Text_register::post_move_processing()
{
    text_req_l_ = 0;
}
