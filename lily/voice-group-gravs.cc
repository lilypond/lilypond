/*
  voicegroup.cc -- implement Voice_group_engravers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "proto.hh"
#include "plist.hh"
#include "musical-request.hh"
#include "voice-group-gravs.hh"
#include "engraver.hh"
#include "command-request.hh"
#include "debug.hh"


Voice_group_engravers::Voice_group_engravers()
{
  dir_ =CENTER;
}

bool
Voice_group_engravers::do_try_request (Request*r_l)
{
  Command_req* c_l = r_l->command();
  if (c_l&& c_l->groupfeature()) 
    {
      Feature f;
      f.type_ = c_l->groupfeature()->type_str_;
      f.value_ = c_l->groupfeature()->value_str_;
      set_feature (f);
      return true;
    }
  return Engraver_group_engraver::do_try_request (r_l);
}



IMPLEMENT_IS_TYPE_B1(Voice_group_engravers,Engraver_group_engraver);

void
Voice_group_engravers::do_print() const
{
#ifndef NPRINT
  Engraver_group_engraver::do_print();
#endif
}



Scalar
Voice_group_engravers::get_feature (String f)
{
  if (f == "vdir")
    return dir_;
  Engraver_group_engraver::get_feature (f);
}

ADD_THIS_ENGRAVER(Voice_group_engravers);
