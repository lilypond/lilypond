//
// lilypond-staff.cc -- implement Lilypond_staff
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

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

  //    Rational mom = items.top ()->at_mom ();
  Rational mom = 0;

  for (Cons<Lilypond_item>** pp = &items.head_; *pp;)
    {
      Cons<Lilypond_item>* i = *pp;
      if (i->car_->at_mom () > mom)
	{
	  if (no_rests_b_g && voice_p->last_note_l_)
	    {
	      voice_p->last_note_l_->end_column_l_ = i->car_->lilypond_column_l_;
	    }
	  else
	    {
	      /* uh, what about quantisation?  This should probably
	         use  mom2standardised_dur ()
		 arg, urg: skip should get duration from start/end columns!
	   	*/

	      Rational r = i->car_->at_mom () - mom;
	      // ugh, need score
	      Lilypond_column* start = lilypond_score_l_g->find_column_l (mom);
	      voice_p->add_item (new Lilypond_skip (start, r));
	    }

	  mom = i->car_->at_mom ();
	  continue;		// unnecessary
	}
      
      Link_array<Lilypond_item> now_items;
      for (Cons<Lilypond_item> *cp = i; cp && cp->car_->at_mom () == mom; cp = cp->next_)
	now_items.push (i->car_);

#if 0
      /*
        Why don't we use <note>, if voice has:

	  <note> <key-change>

        we'd get last_item == key_change -> last_note == 0;
	*/
      Lilypond_note * last_note = dynamic_cast<Lilypond_note*> (voice_p->last_item_l_);
#else
      /*
        Not sure, is this better?
       */
      Lilypond_note * last_note = voice_p->last_note_l_;
#endif

      Link_array<Lilypond_item> candidates; 

      for (int i=0; last_note && i < now_items.size (); i++)
	{
	  Lilypond_note * now_note = dynamic_cast<Lilypond_note*> (now_items[i]);
	  if (now_note && last_note->channel_i_ != now_note->channel_i_)
	    candidates.push (now_note);
	}

      if (candidates.size())
	{
	  now_items = candidates;
	}

      Lilypond_item * which = 0;
      if (now_items.size () > 1)
	{
	  int mindiff = 100000;	// ugh
	  for (int i=0; last_note && i < now_items.size (); i++)
	    {
	      Lilypond_note *nt = dynamic_cast<Lilypond_note*> (now_items[i]);
	      if (!nt)
		continue;
	      int diff = abs (last_note->pitch_i_ - nt->pitch_i_ );
	      if(diff < mindiff)
		{
		  mindiff =  diff;
		  which = now_items [i];
		}
	    }

	  if (which && mindiff > 18)		// more than 1.5 octaves apart.  Don't put in same voice.
	    {
	      which =0;
	    }
	}
      else if (now_items.size () == 1)
	which = now_items[0];
      
      if (which)
	{
	  while ((*pp)->car_ != which)
	    pp = &(*pp)->next_;
      
	  mom += (*pp)->car_->duration_mom ();
	  Cons<Lilypond_item>* c = items.remove_cons (pp);
	  voice_p->add_item (c->car_);
	  delete c;
	}
      else 
	{
	  pp = &(*pp)->next_;
	  continue;
	}
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

      trackbody += "\\"  + voicename + "\n";

      lilypond_stream_r << '\n';
      i->car_->output (lilypond_stream_r);
      c++;      
    }

  lilypond_stream_r << _ ("% MIDI copyright:") << copyright_str_ << '\n';
  lilypond_stream_r << _ ("% MIDI instrument:") << instrument_str_ << '\n';
  lilypond_stream_r << id_str () << " = ";
  lilypond_stream_r << "<\n " << trackbody << " >\n";

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


#if 0 // not used for now
void
Lilypond_staff::output_lilypond_rest (Lilypond_stream& lilypond_stream_r, Rational begin_mom, Rational end_mom)
{
  Rational bar_mom = lilypond_time_signature_l_->bar_mom ();
  Rational now_mom = begin_mom;

  int begin_bar_i = (int) (now_mom / bar_mom) + 1;
  int end_bar_i = (int) (end_mom / bar_mom) + 1;

  if (end_bar_i == begin_bar_i)
    {
      output_lilypond_rest_remain (lilypond_stream_r, end_mom - begin_mom);
      return;
    }

  // multiple bars involved
  int bar_i = (int) (now_mom / bar_mom) + 1;

  //fill current bar
  Rational begin_bar_mom = Rational (begin_bar_i - 1) * bar_mom;
  if (now_mom > begin_bar_mom)
    {
      int next_bar_i = (int) (now_mom / bar_mom) + 2;
      Rational next_bar_mom = Rational (next_bar_i - 1) * bar_mom;
      assert (next_bar_mom <= end_mom);

      Rational remain_mom = next_bar_mom - now_mom;
      if (remain_mom > Rational (0))
	{
	  output_lilypond_rest_remain (lilypond_stream_r, remain_mom);
	  now_mom += remain_mom;
	}

      bar_i = check_end_bar_i (now_mom, bar_i);
    }

  // fill whole bars
  int count_i = end_bar_i - bar_i;
  for (int i = 0; i < count_i; i++)
    {
      int begin_bar_i = check_begin_bar_i (now_mom, bar_i);
      if (begin_bar_i)
	output_lilypond_begin_bar (lilypond_stream_r, now_mom, begin_bar_i);
      lilypond_stream_r << "r1 ";
      //	*lilypond_stream_r.os_p_ << flush;
      if (begin_bar_i)
	LOGOUT (NORMAL_ver) << begin_bar_i << flush;
      bar_i = check_end_bar_i (now_mom, bar_i);
      now_mom += bar_mom;
    }

  // use "int i" here, and gcc 2.7.2 hits internal compiler error
  int ii = check_begin_bar_i (now_mom, bar_i);
  if (ii)
    output_lilypond_begin_bar (lilypond_stream_r, now_mom, ii);

  //    bar_i = check_end_bar_i (now_mom, bar_i);

  Rational remain_mom = end_mom - Rational (end_bar_i - 1) * bar_mom;
  if (remain_mom > Rational (0))
    {
      output_lilypond_rest_remain (lilypond_stream_r, remain_mom);
      now_mom += remain_mom;
    }
  assert (now_mom == end_mom);
}

void
Lilypond_staff::output_lilypond_rest_remain (Lilypond_stream& lilypond_stream_r, Rational mom)
{
  if (Duration_convert::no_quantify_b_s)
    {
      Duration dur = Duration_convert::mom2_dur (mom);
      lilypond_stream_r << "r" << dur.str () << " ";
      //	assert (mom == dur.mom ());
      assert (mom == dur.length ());
      return;
    }

  Duration dur = Duration_convert::mom2standardised_dur (mom);
  if (dur.type_i_>-10)
    lilypond_stream_r << "r" << dur.str () << " ";
}
#endif


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
