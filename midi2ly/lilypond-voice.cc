//
// lilypond-voice.cc -- implement Lilypond_voice
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include "string-convert.hh"
#include "midi2ly-global.hh"
#include "lilypond-column.hh"
#include "lilypond-item.hh"
#include "lilypond-staff.hh"
#include "lilypond-stream.hh"
#include "lilypond-voice.hh"
#include "lilypond-score.hh"

extern Lilypond_score* lilypond_score_l_g;

Lilypond_voice::Lilypond_voice (Lilypond_staff* lilypond_staff_l)
{
  lilypond_staff_l_ = lilypond_staff_l;
  last_item_l_ =0;
  last_note_l_ =0;
}

void
Lilypond_voice::add_item (Lilypond_item* lilypond_item_l)
{
  last_item_l_  = lilypond_item_l;
  if (Lilypond_note* n = dynamic_cast<Lilypond_note*> (lilypond_item_l))
    {
      last_note_l_  = n;
    }
  lilypond_item_l_list_.append (new Cons<Lilypond_item> (lilypond_item_l, 0));
}

/**
   analyse pitches to determine clef.
 */
String
Lilypond_voice::get_clef () const
{
  Lilypond_note * n =0;

  for (Cons<Lilypond_item> *cp = lilypond_item_l_list_.head_; !n && cp; cp = cp->next_)
    {
      n = dynamic_cast<Lilypond_note*> (cp->car_);
    }
  
  if (!n)
    return "";

  int p = n->pitch_i_;

  if (p < 56)
    return "\\clef \"bass\"\n";
  else if (p > 67)
    return "\\clef \"treble\"\n";
  else
    return "";
}

static int const FAIRLY_LONG_VOICE_i = 6;

void
Lilypond_voice::output (Lilypond_stream& lilypond_stream_r)
{
  lilypond_stream_r << "{ ";
  if (lilypond_item_l_list_.size_i () > FAIRLY_LONG_VOICE_i)
    lilypond_stream_r << '\n';


  lilypond_stream_r << get_clef () << '\n';
  
  int current_bar_i = 0;
  Rational bar_mom = lilypond_staff_l_->lilypond_time_signature_l_->bar_mom ();

  for (Cons<Lilypond_item>* i = lilypond_item_l_list_.head_; i; i = i->next_)
    {
      Rational at_mom = i->car_->lilypond_column_l_->at_mom ();
      int bar_i = (int) (at_mom / bar_mom) + 1;
      if (bar_i > current_bar_i) 
	{
	  if (current_bar_i) 
	    {
	      if (at_mom == Rational (bar_i - 1) * bar_mom)
		lilypond_stream_r << "|";
	      lilypond_stream_r << "\n% ";
	      lilypond_stream_r << String_convert::i2dec_str (bar_i, 0, ' ');
	      lilypond_stream_r << '\n';
	    }
	  LOGOUT (NORMAL_ver) << "[" << bar_i << "]" << flush; 
	  current_bar_i = bar_i;
	}

      lilypond_stream_r << *i->car_;
      if (Lilypond_key* k = dynamic_cast<Lilypond_key*> (i->car_))
	lilypond_staff_l_->lilypond_key_l_ = lilypond_score_l_g->lilypond_key_l_ = k;
    }

  if (lilypond_item_l_list_.size_i () > FAIRLY_LONG_VOICE_i)
    lilypond_stream_r << '\n';

  lilypond_stream_r << "} ";
}


