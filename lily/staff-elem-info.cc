/*
  staff-elem-info.cc -- implement Staff_elem_info

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staff-elem-info.hh"
#include "request.hh"

Staff_elem_info::Staff_elem_info(Staff_elem*s_l, Request*r_l)
{
    elem_p_ = s_l;
    voice_l_ =  (r_l)?r_l->voice_l():0;
    req_l_ = r_l;
}

Staff_elem_info::Staff_elem_info()
{
    elem_p_ = 0;
    voice_l_ = 0;

    req_l_ = 0;
}

Features::Features()
{
    direction_i_ = 0;
    initialiser_b_ = false;
}

Features
Features::dir(int d) return f;
{
    f.initialiser_b_ = true;
    f.direction_i_ = d;
}
