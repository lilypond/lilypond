//
// lilypond-voice.cc -- implement Lilypond_voice
//
// (c) 1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>

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
  threads_.push (new Cons_list<Lilypond_item>);
  mom_ = 0;
}
  
void
Lilypond_voice::add_items (Link_array<Lilypond_item>& items)
{
  int thread = 0;
  for (int i = 0; i < items.size (); i++)
    {
      Lilypond_item* item = items[i];

      int to_thread;
      if (Lilypond_note* n = dynamic_cast<Lilypond_note*> (item))
	to_thread = thread++;
      else
	to_thread = 0;
      
      if (to_thread >= threads_.size ())
	threads_.push (new Cons_list<Lilypond_item>);
      
      if (to_thread == 0 && item->at_mom () > mom_)
	{
	  /* urg: skip should use refer to end-colum, not separate moment */
	  Rational r = item->at_mom () - mom_;
	  Lilypond_column* start = lilypond_score_l_g->find_column_l (mom_);
	  threads_[to_thread]->append (new Cons<Lilypond_item> (new Lilypond_skip (start, r), 0));
	  mom_ = item->at_mom ();
	}

      threads_[to_thread]->append (new Cons<Lilypond_item> (item, 0));
      if (to_thread == 0)
	mom_ += item->duration_mom ();
    }
}

/**
   analyse pitches to determine clef.
 */
String
Lilypond_voice::get_clef () const
{
  Lilypond_note * n =0;

  for (Cons<Lilypond_item> *cp = threads_[0]->head_; !n && cp; cp = cp->next_)
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
  if (threads_[0]->size_i () > FAIRLY_LONG_VOICE_i)
    lilypond_stream_r << '\n';


  lilypond_stream_r << get_clef () << '\n';
  
  int current_bar_i = 0;
  Rational bar_mom = lilypond_staff_l_->lilypond_time_signature_l_->bar_mom ();

  Link_array <Cons<Lilypond_item> > heads;
  for (int i = 1; i < threads_.size (); i++)
    heads.push (threads_[i]->head_);
  for (Cons<Lilypond_item>* i = threads_[0]->head_; i; i = i->next_)
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

      if (dynamic_cast<Lilypond_note*> (i->car_)
	  && heads.size ()
	  && heads[0]
	  && heads[0]->car_->at_mom () == at_mom)
	{
	  lilypond_stream_r << '<';
      
	  lilypond_stream_r << *i->car_;

	  for (int h = 0;
	       h < heads.size ()
		 && heads[h]
		 && heads[h]->car_->at_mom () == at_mom;
	       h++)
	    {
	      lilypond_stream_r << *heads[h]->car_;
	      heads[h] = heads[h]->next_;
	    }
	  lilypond_stream_r << '>';
	}
      else
	lilypond_stream_r << *i->car_;
      
      if (Lilypond_key* k = dynamic_cast<Lilypond_key*> (i->car_))
	lilypond_staff_l_->lilypond_key_l_ = lilypond_score_l_g->lilypond_key_l_ = k;
    }

  if (threads_[0]->size_i () > FAIRLY_LONG_VOICE_i)
    lilypond_stream_r << '\n';

  lilypond_stream_r << "} ";
}


