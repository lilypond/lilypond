/*
  extender-engraver.cc -- implement Extender_engraver

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "extender-engraver.hh"
#include "extender-spanner.hh"
#include "text-def.hh"
#include "score-column.hh"
#include "g-text-item.hh"

ADD_THIS_TRANSLATOR (Extender_engraver);

Extender_engraver::Extender_engraver ()
{
  extender_spanner_p_ = 0;
  req_l_ = 0;
}

void
Extender_engraver::acknowledge_element (Score_element_info i)
{
  if (G_text_item* t = dynamic_cast<G_text_item*> (i.elem_l_))
    {
      Rhythmic_req * rh = dynamic_cast<Rhythmic_req*>  (i.req_l_);
      if (!rh)
	return;

      now_lyrics_.push (Text_lyric_tuple (t, rh, now_mom () + rh->length_mom ()));
      /*
	UGH.  What do we do in case of multiple alternatives? 
       */
      if (extender_spanner_p_
	  && !extender_spanner_p_->spanned_drul_[RIGHT]
	    )
	  {
	    extender_spanner_p_->set_textitem (RIGHT, t);
	  }
    }
}


bool
Extender_engraver::do_try_music (Music* req_l)
{
  if (Extender_req* p = dynamic_cast <Extender_req *> (req_l))
    {
      if (req_l_)
	return false;

      req_l_ = p;
      return true;
    }
  return false;
}

void
Extender_engraver::do_removal_processing ()
{
  if (extender_spanner_p_)
    {
      req_l_->warning (_ ("unterminated extender"));
      extender_spanner_p_->set_bounds(RIGHT, get_staff_info ().command_pcol_l ());
    }
}

void
Extender_engraver::do_process_requests ()
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
	  req_l_->warning ("Nothing to connect extender to on the left. Ignoring extender request");
	  return;
	}
      
      extender_spanner_p_ = new Extender_spanner;
      extender_spanner_p_->set_textitem  (LEFT, stopped_texts[0].text_l_);
      announce_element (Score_element_info (extender_spanner_p_, req_l_));
    }
}


void
Extender_engraver::do_pre_move_processing ()
{
  for (int i=0; i < now_lyrics_.size (); i++)
    {
      past_lyrics_pq_.insert (now_lyrics_[i]);
    }
  now_lyrics_.clear ();

  if (extender_spanner_p_)
    {
      typeset_element (extender_spanner_p_);
      extender_spanner_p_ = 0;
    }
}
void
Extender_engraver::do_post_move_processing ()
{
  Moment now = now_mom ();
  while (past_lyrics_pq_.size () && past_lyrics_pq_.front ().end_ < now)
    past_lyrics_pq_.delmin ();

  req_l_ =0;
}

/**********************************************************************/
Text_lyric_tuple::Text_lyric_tuple ()
{
  text_l_ =0;
  req_l_ =0;
  end_ = 0;
}

Text_lyric_tuple::Text_lyric_tuple (G_text_item *h, Rhythmic_req*m, Moment mom)
{
  text_l_ = h;
  req_l_ = m;
  end_ = mom;
}


int
Text_lyric_tuple::time_compare (Text_lyric_tuple const&h1,
			     Text_lyric_tuple const &h2)
{
  return (h1.end_ - h2.end_ ).sign ();
}
