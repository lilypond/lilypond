/*
  textreg.cc -- implement Text_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musicalrequest.hh"
#include "textreg.hh"
#include "textitem.hh"

Text_register::Text_register(Complex_walker*w)
    : Request_register(w)
{
    text_p_ = 0;
    set_dir(0);
}

bool
Text_register::try_request(Request*req_l)
{
    if (!req_l->text())
	return false;
    if (accepted_req_arr_.size() &&
	Text_req::compare(*req_l->text(), *accepted_req_arr_[0]->text()))

	return false;

    accepted_req_arr_.push(req_l);
    return true;
}

void
Text_register::process_request()
{
    
    if (accepted_req_arr_.size()) {
	text_p_ = new Text_item(accepted_req_arr_[0]->text(), 10);
	announce_element(Staff_elem_info(text_p_, accepted_req_arr_[0], this));
    }
}
void
Text_register::do_pre_move_process()
{
    if (text_p_) {
	text_p_->dir_i_ = dir_i_;
	typeset_element(text_p_);
	text_p_ = 0;
    }
}
void
Text_register::set_dir(int i)
{
    dir_i_ = i;
}
