/*
  voice-gravs.cc -- implement Voice_engravers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "voice-gravs.hh"
#include "engraver.hh"
#include "debug.hh"


ADD_THIS_ENGRAVER(Voice_engravers);
IMPLEMENT_IS_TYPE_B1(Voice_engravers, Engraver_group_engraver);
IMPLEMENT_STATIC_NAME(Voice_engravers);

bool
Voice_engravers::interpret_request_b(Request*r)
{
    return try_request(r);
}

