/*
  hyphen-engraver.cc -- implement Hyphen_engraver

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "hyphen-engraver.hh"
#include "hyphen-spanner.hh"
#include "score-column.hh"
#include "text-item.hh"
#include "extender-engraver.hh"

ADD_THIS_TRANSLATOR (Hyphen_engraver);

Hyphen_engraver::Hyphen_engraver ()
{
  hyphen_spanner_p_ = 0;
  req_l_ = 0;
}

void
Hyphen_engraver::acknowledge_element (Score_element_info i)
{
  if (Text_item* t = dynamic_cast<Text_item*> (i.elem_l_))
    {
      Rhythmic_req * rh = dynamic_cast<Rhythmic_req*>  (i.req_l_);
      if (!rh)
	return;

      now_lyrics_.push (Text_lyric_tuple (t, rh, now_mom () + rh->length_mom ()));
      /*
	UGH.  What do we do in case of multiple alternatives? 
       */
      if (hyphen_spanner_p_
	  && !hyphen_spanner_p_->spanned_drul_[RIGHT]
	    )
	  {
	    hyphen_spanner_p_->set_textitem (RIGHT, t);
	  }
    }
}


bool
Hyphen_engraver::do_try_music (Music* r)
{
  if (Hyphen_req* p = dynamic_cast <Hyphen_req *> (r))
    {
      if (req_l_)
	return false;

      req_l_ = p;
      return true;
    }
  return false;
}

void
Hyphen_engraver::do_removal_processing ()
{
  if (hyphen_spanner_p_)
    {
      req_l_->warning (_ ("unterminated hyphen"));
      hyphen_spanner_p_->set_bounds(RIGHT, get_staff_info ().command_pcol_l ());
    }
}

void
Hyphen_engraver::do_process_requests ()
{
  Array<Text_lyric_tuple> stopped_texts;
  Moment now = now_mom ();

  stopped_texts.clear ();
  while (past_lyrics_pq_.size ()
	 && past_lyrics_pq_.front ().end_ == now)
    stopped_texts.push (past_lyrics_pq_.get ());

  if (req_l_)
    {
      if (!stopped_texts.size ())
	{
	  req_l_->warning ("Nothing to connect hyphen to on the left. Ignoring hyphen request");
	  return;
	}
      
      hyphen_spanner_p_ = new Hyphen_spanner;
      hyphen_spanner_p_->set_textitem  (LEFT, stopped_texts[0].text_l_);
      announce_element (Score_element_info (hyphen_spanner_p_, req_l_));
    }
}


void
Hyphen_engraver::do_pre_move_processing ()
{
  for (int i=0; i < now_lyrics_.size (); i++)
    {
      past_lyrics_pq_.insert (now_lyrics_[i]);
    }
  now_lyrics_.clear ();

  if (hyphen_spanner_p_)
    {
      typeset_element (hyphen_spanner_p_);
      hyphen_spanner_p_ = 0;
    }
}
void
Hyphen_engraver::do_post_move_processing ()
{
  Moment now = now_mom ();
  while (past_lyrics_pq_.size () && past_lyrics_pq_.front ().end_ < now)
    past_lyrics_pq_.delmin ();

  req_l_ =0;
}


