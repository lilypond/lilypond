//
// mudela-voice.cc -- implement Mudela_voice
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include "string-convert.hh"
#include "mi2mu-global.hh"
#include "mudela-column.hh"
#include "mudela-item.hh"
#include "mudela-staff.hh"
#include "mudela-stream.hh"
#include "mudela-voice.hh"

Mudela_voice::Mudela_voice (Mudela_staff* mudela_staff_l)
{
  mudela_staff_l_ = mudela_staff_l;
}

void
Mudela_voice::add_item (Mudela_item* mudela_item_l)
{
  mudela_item_l_list_.bottom().add (mudela_item_l);
}

Moment 
Mudela_voice::begin_mom()
{
  return mudela_item_l_list_.size() ? 
    mudela_item_l_list_.top()->at_mom() : Moment (0);
}

Moment 
Mudela_voice::end_mom()
{
  return mudela_item_l_list_.size() ? 
    mudela_item_l_list_.bottom()->at_mom() : Moment (0);
}

static int const FAIRLY_LONG_VOICE_i = 6;

void
Mudela_voice::output (Mudela_stream& mudela_stream_r)
{
  if  (!mudela_item_l_list_.size())
    return;
  
  if  (mudela_item_l_list_.size() > FAIRLY_LONG_VOICE_i)
    mudela_stream_r << '\n';

  int current_bar_i = 0;
  Moment bar_mom = mudela_staff_l_->mudela_time_signature_l_->bar_mom();

  for  (PCursor<Mudela_item*> i (mudela_item_l_list_); i.ok(); i++) 
    {
      Moment at_mom = i->mudela_column_l_->at_mom();
      int bar_i = (int) (at_mom / bar_mom) + 1;
      if  (bar_i > current_bar_i) 
	{
	  if  (current_bar_i) 
	    {
	      if  (at_mom == Moment (bar_i - 1) * bar_mom)
		mudela_stream_r << "|";
	      mudela_stream_r << "\n% ";
	      mudela_stream_r << String_convert::i2dec_str (bar_i, 0, ' ');
	      mudela_stream_r << '\n';
	    }
	  LOGOUT(NORMAL_ver) << "[" << bar_i << "]" << flush; 
	  current_bar_i = bar_i;
	}

      mudela_stream_r << **i;
    }

  if  (mudela_item_l_list_.size() > FAIRLY_LONG_VOICE_i)
    mudela_stream_r << '\n';
}

