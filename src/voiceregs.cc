/*
  voiceregs.cc -- implement Voice_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musicalrequest.hh"
#include "voiceregs.hh"
#include "register.hh"
#include "slurreg.hh"
#include "headreg.hh"

Voice_registers::Voice_registers(Complex_walker*c_l, Voice *v_p)
{
    voice_l_ = v_p;
    add(new Notehead_register(c_l));
    add(new Slur_register(c_l));
}

void
Voice_registers::acknowledge_element(Staff_elem_info i)
{
    if (i.voice_l_ != voice_l_)
	return;
    Register_group::acknowledge_element(i);
}

bool
Voice_registers::acceptable_request_b(Request*r)
{
    return (r->rest() || r->note() || r->slur());
}
