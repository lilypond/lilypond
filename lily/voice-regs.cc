/*
  voice-regs.cc -- implement Voice_registers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "voice-regs.hh"
#include "register.hh"
#include "debug.hh"

Voice_registers::Voice_registers()
{
}

void
Voice_registers::do_print() const
{
#ifndef NPRINT
    Register_group_register::do_print();
#endif
}

ADD_THIS_REGISTER(Voice_registers);
IMPLEMENT_IS_TYPE_B1(Voice_registers, Register_group_register);
IMPLEMENT_STATIC_NAME(Voice_registers);

bool
Voice_registers::interpret_request_b(Request*r)
{
    return try_request(r);
}

