/*
  staff-gravs.cc -- implement Staff_engravers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staff-sym.hh"
#include "voice-group-gravs.hh"
#include "voice-gravs.hh"
#include "staff-gravs.hh"
#include "command-request.hh"
#include "bar.hh"
#include "debug.hh"
#include "input-engraver.hh"
#include "meter-grav.hh"
#include "staffline.hh"


Staff_engravers::Staff_engravers()
{
    staffline_p_ =0;
}

void
Staff_engravers::typeset_element(Score_elem *elem_p)
{
    staff_elem_l_arr_.push(elem_p);
    Engraver_group_engraver::typeset_element(elem_p);
}

void
Staff_engravers::typeset_breakable_item( Item * nobreak_p)
{
    staff_elem_l_arr_.push(nobreak_p);
    Engraver_group_engraver::typeset_breakable_item(nobreak_p);
}

void
Staff_engravers::do_pre_move_processing()
{
    Engraver_group_engraver::do_pre_move_processing();
    group_staff_elems();
}

void
Staff_engravers::group_staff_elems()
{
    for (int i=0; i <staff_elem_l_arr_.size(); i++)
	staffline_p_->add_element(staff_elem_l_arr_[i]);
    staff_elem_l_arr_.set_size(0);
}

void
Staff_engravers::do_removal_processing()
{
    /* this is a "destructor type function", first do children, then self. */
    Engraver_group_engraver::do_removal_processing();
    group_staff_elems();

    staffline_p_->right_col_l_ = get_staff_info().command_pcol_l();
    Request_engraver::typeset_element(staffline_p_);
    staffline_p_ = 0;
}

void
Staff_engravers::do_creation_processing()
{
    staffline_p_ = new Line_of_staff;
    staffline_p_->left_col_l_ = get_staff_info().command_pcol_l();

    // don't broadcast to self.
    Request_engraver::announce_element(Score_elem_info(staffline_p_,0));
    Engraver_group_engraver::do_creation_processing();
}


IMPLEMENT_STATIC_NAME(Staff_engravers);
IMPLEMENT_IS_TYPE_B1(Staff_engravers,Engraver_group_engraver);
ADD_THIS_ENGRAVER(Staff_engravers);

