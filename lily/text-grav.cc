/*
  text-reg.cc -- implement Text_engraver

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  Obsolete.
*/
#include "note-column.hh"
#include "musical-request.hh"
#include "text-grav.hh"
#include "text-item.hh"

Text_engraver::Text_engraver()
{
    text_p_ = 0;
    dir_i_ =0;
    do_post_move_processing();
}

bool
Text_engraver::do_try_request(Request*req_l)
{
    Musical_req *m = req_l->musical();
    if (!m || ! m->text())
	return false;
    if (text_req_l_ &&
	Text_req::compare(*m->text(), *text_req_l_))

	return false;

    text_req_l_ = m->text();
    return true;
}
void
Text_engraver::acknowledge_element(Score_elem_info i)
{
    if (text_p_ && i.elem_l_->name() == Note_column::static_name()) {
	text_p_->add_support(i.elem_l_);
    }
}
void
Text_engraver::do_process_requests()
{
    if (text_req_l_) {
	text_p_ = new Text_item(text_req_l_->tdef_p_, text_req_l_->dir_i_); // ugh
	announce_element(Score_elem_info(text_p_, text_req_l_));
    }
}
void
Text_engraver::do_pre_move_processing()
{
    if (text_p_) {
	if (dir_i_ && !text_p_->dir_i_)
	    text_p_->dir_i_ = dir_i_;
	Staff_symbol* s_l = get_staff_info().staff_sym_l_;
	text_p_->set_staffsym(s_l);
	typeset_element(text_p_);
	    
	text_p_ = 0;
    }
}
void
Text_engraver::set_feature(Feature i)
{
    if (i.type_ == "vdir")	
	dir_i_ = i.value_;
}
void
Text_engraver::do_post_move_processing()
{
    text_req_l_ = 0;
}

IMPLEMENT_IS_TYPE_B1(Text_engraver,Engraver);
ADD_THIS_ENGRAVER(Text_engraver);
