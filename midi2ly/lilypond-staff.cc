//
// lilypond-staff.cc -- implement Lilypond_staff
//
// (c) 1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>

#include <assert.h>
#include <ctype.h>
#include "rational.hh"
#include "duration-convert.hh"
#include "string-convert.hh"
#include "midi2ly-proto.hh"
#include "midi2ly-global.hh"
#include "lilypond-column.hh"
#include "lilypond-item.hh"
#include "lilypond-staff.hh"
#include "lilypond-stream.hh"
#include "lilypond-voice.hh"
#include "lilypond-score.hh"

#include "killing-cons.tcc"

extern Lilypond_score* lilypond_score_l_g;

Lilypond_staff::Lilypond_staff (int number_i, String copyright_str, String track_name_str, String instrument_str)
{
  number_i_ = number_i;
  copyright_str_ = copyright_str;
  instrument_str_ = instrument_str;
  name_str_ = track_name_str;
  lilypond_key_l_ = 0;
  lilypond_time_signature_l_ = 0;
  lilypond_tempo_l_ = 0;
}

void
Lilypond_staff::add_item (Lilypond_item* lilypond_item_p)
{
  lilypond_item_p_list_.append (new Killing_cons <Lilypond_item> (lilypond_item_p, 0));
  if (lilypond_item_p->lilypond_column_l_)
    lilypond_item_p->lilypond_column_l_->add_item (lilypond_item_p);
}
/**
   Walk ITEMS and find voices.  Remove categorised items.

   TODO: 
   
     * collect all channels into separate voices. Use chords for sim
       notes on same channel.
     * assume voices/assume chords modes.
   
 */
void
Lilypond_staff::eat_voice (Cons_list<Lilypond_item>& items)
{
  Lilypond_voice* voice_p = new Lilypond_voice (this);
  lilypond_voice_p_list_.append (new Killing_cons<Lilypond_voice> (voice_p, 0));

  Rational mom = 0;

  Link_array<Lilypond_item> now_items;
  for (Cons<Lilypond_item>** i = &items.head_; *i;)
    {
      while (*i && (*i)->car_->at_mom () < mom)
	i = &(*i)->next_;
      
      Lilypond_note* last_note = 0;
      Link_array<Lilypond_item> now_items;
      if (*i)
	mom = (*i)->car_->at_mom ();
      while (*i && (*i)->car_->at_mom () == mom)
	{
	  Lilypond_note* note = dynamic_cast<Lilypond_note*> ((*i)->car_);
	  if (note && last_note
	      /* ugh, should sort out (whether to) channel before */
	      && (note->channel_i_ != last_note->channel_i_
		  || (note->duration_mom ()
		      != last_note->duration_mom ())))
	    break;
	  Cons<Lilypond_item>* c = items.remove_cons (i);
	  now_items.push (c->car_);
	  if (note)
	    last_note = note;
	  delete c;
	}
      
      if (now_items.size ())
	mom = now_items.top ()->at_mom ();
      if (last_note)
	mom += last_note->duration_mom ();

      voice_p->add_items (now_items);
    }
}

String
Lilypond_staff::id_str ()
{
  String id (name_str ());
  char *cp = id.ch_l ();
  char *end = cp + id.length_i ();
  for (;cp < end; cp++)
    {
      if (!isalpha (*cp))
	{
	  *cp = 'X';
	}
    }
  return id;
}

String
Lilypond_staff::name_str ()
{
  if (name_str_.length_i ())
    return name_str_;
  return String ("track") + to_str (char ('A' - 1 + number_i_));
}



void
Lilypond_staff::output (Lilypond_stream& lilypond_stream_r)
{
  int c =0;
  
  String trackbody = "";
  for (Cons<Lilypond_voice>* i = lilypond_voice_p_list_.head_; i; i = i->next_)
    {
      String voicename = id_str () + "voice" + to_str (char (c + 'A'));
      
      lilypond_stream_r << voicename << " = \\notes ";

      trackbody += "\\context Voice = " + voicename + " \\"  + voicename + "\n";
      lilypond_stream_r << '\n';
      i->car_->output (lilypond_stream_r);
      c++;      
      lilypond_stream_r << '\n';
    }

  lilypond_stream_r << '\n';
  lilypond_stream_r << _ ("% MIDI copyright:") << copyright_str_ << '\n';
  lilypond_stream_r << _ ("% MIDI instrument:") << instrument_str_ << '\n';
  lilypond_stream_r << id_str () << " = ";
  lilypond_stream_r << "<\n" << trackbody << ">\n";

  lilypond_stream_r << " % " << name_str () << '\n';
}

void
Lilypond_staff::output_lilypond_begin_bar (Lilypond_stream& lilypond_stream_r, Rational now_mom, int bar_i)
{
  Rational bar_mom = lilypond_time_signature_l_->bar_mom ();
  Rational into_bar_mom = now_mom - Rational (bar_i - 1) * bar_mom;
  if (bar_i > 1)
    {
      if (!into_bar_mom)
	lilypond_stream_r << "|\n";
    }
  lilypond_stream_r << "% " << String_convert::i2dec_str (bar_i, 0, ' ');
  if (into_bar_mom)
    lilypond_stream_r << ":" << Duration_convert::dur2_str (Duration_convert::mom2_dur (into_bar_mom));
  lilypond_stream_r << '\n';
}


void
Lilypond_staff::process ()
{
  /*
    group items into voices
  */

  assert (lilypond_score_l_g);
  lilypond_key_l_ = lilypond_score_l_g->lilypond_key_l_;
  lilypond_time_signature_l_ = lilypond_score_l_g->lilypond_time_signature_l_;
  lilypond_tempo_l_ = lilypond_score_l_g->lilypond_tempo_l_;

  Cons_list<Lilypond_item> items;
  for (Cons<Lilypond_item>* i = lilypond_item_p_list_.head_; i; i = i->next_)
    items.append (new Cons<Lilypond_item> (i->car_, 0));

  while (items.size_i ())
    eat_voice (items);
}
