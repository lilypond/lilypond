/*
  staffeleminfo.cc -- implement Staff_elem_info

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staffeleminfo.hh"
#include "request.hh"
#include "voice.hh"

Staff_elem_info::Staff_elem_info(Staff_elem*s_l, Request*r_l,
				 Request_register *reg_l)
{
    elem_p_ = s_l;
    voice_l_ =  (r_l)?r_l->elt_l_->voice_l_:0;
    req_l_ = r_l;
    group_regs_l_ = 0;
    origin_reg_l_ = reg_l;
}

Staff_elem_info::Staff_elem_info()
{
    elem_p_ = 0;
    voice_l_ = 0;

    group_regs_l_ = 0;
    origin_reg_l_ = 0;
    req_l_ = 0;
}

