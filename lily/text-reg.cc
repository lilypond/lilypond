/*
  text-reg.cc -- implement Text_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "note-column.hh"
#include "musical-request.hh"
#include "text-reg.hh"
#include "text-item.hh"

Text_register::Text_register()
{
    text_p_ = 0;
    dir_i_ =0;
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
Text_register::acknowledge_element(Staff_elem_info i)
{
    if (text_p_ && i.elem_l_->name() == Note_column::static_name()) {
	text_p_->add_support(i.elem_l_);
    }
}
void
Text_register::process_requests()
{
    if (text_req_l_) {
	text_p_ = new Text_item(text_req_l_); // UGH
	announce_element(Staff_elem_info(text_p_, text_req_l_));
    }
}
void
Text_register::pre_move_processing()
{
    if (text_p_) {
	text_p_->dir_i_ = dir_i_;
	Staff_symbol* s_l = get_staff_info().staff_sym_l_;
	text_p_->set_staffsym(s_l);
	typeset_element(text_p_);
	    
	text_p_ = 0;
    }
}
void
Text_register::set_feature(Feature i)
{
    if (i.type_ == "vdir")	
	dir_i_ = i.value_;
}
void
Text_register::post_move_processing()
{
    text_req_l_ = 0;
}
IMPLEMENT_STATIC_NAME(Text_register);
ADD_THIS_REGISTER(Text_register);
