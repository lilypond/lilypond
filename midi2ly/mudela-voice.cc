//
// mudela-voice.cc -- implement Mudela_voice
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include "string-convert.hh"
#include "midi2ly-global.hh"
#include "mudela-column.hh"
#include "mudela-item.hh"
#include "mudela-staff.hh"
#include "mudela-stream.hh"
#include "mudela-voice.hh"
#include "mudela-score.hh"

extern Mudela_score* mudela_score_l_g;

Mudela_voice::Mudela_voice (Mudela_staff* mudela_staff_l)
{
  mudela_staff_l_ = mudela_staff_l;
  last_item_l_ =0;
  last_note_l_ =0;
}

void
Mudela_voice::add_item (Mudela_item* mudela_item_l)
{
  last_item_l_  = mudela_item_l;
  if (Mudela_note* n = dynamic_cast<Mudela_note*> (mudela_item_l))
    {
      last_note_l_  = n;
    }
  mudela_item_l_list_.append (new Cons<Mudela_item> (mudela_item_l, 0));
}

/**
   analyse pitches to determine clef.
 */
String
Mudela_voice::get_clef () const
{
  Mudela_note * n =0;

  for (Cons<Mudela_item> *cp = mudela_item_l_list_.head_; !n && cp; cp = cp->next_)
    {
      n = dynamic_cast<Mudela_note*> (cp->car_);
    }
  
  if (!n)
    return "";

  int p = n->pitch_i_;

  if (p < 56)
    return "\\clef \"bass\";\n";
  else if (p > 67)
    return "\\clef \"treble\";\n";
  else
    return "";
}

static int const FAIRLY_LONG_VOICE_i = 6;

void
Mudela_voice::output (Mudela_stream& mudela_stream_r)
{
  mudela_stream_r << "{ ";
  if (mudela_item_l_list_.size_i () > FAIRLY_LONG_VOICE_i)
    mudela_stream_r << '\n';


  mudela_stream_r << get_clef () << '\n';
  
  int current_bar_i = 0;
  Rational bar_mom = mudela_staff_l_->mudela_time_signature_l_->bar_mom ();

  for (Cons<Mudela_item>* i = mudela_item_l_list_.head_; i; i = i->next_)
    {
      Rational at_mom = i->car_->mudela_column_l_->at_mom ();
      int bar_i = (int) (at_mom / bar_mom) + 1;
      if (bar_i > current_bar_i) 
	{
	  if (current_bar_i) 
	    {
	      if (at_mom == Rational (bar_i - 1) * bar_mom)
		mudela_stream_r << "|";
	      mudela_stream_r << "\n% ";
	      mudela_stream_r << String_convert::i2dec_str (bar_i, 0, ' ');
	      mudela_stream_r << '\n';
	    }
	  LOGOUT (NORMAL_ver) << "[" << bar_i << "]" << flush; 
	  current_bar_i = bar_i;
	}

      mudela_stream_r << *i->car_;
      if (Mudela_key* k = dynamic_cast<Mudela_key*> (i->car_))
	mudela_staff_l_->mudela_key_l_ = mudela_score_l_g->mudela_key_l_ = k;
    }

  if (mudela_item_l_list_.size_i () > FAIRLY_LONG_VOICE_i)
    mudela_stream_r << '\n';

  mudela_stream_r << "} ";
}


