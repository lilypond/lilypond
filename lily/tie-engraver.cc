/*   
  ctie-engraver.cc --  implement Tie_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "tie-engraver.hh"
#include "command-request.hh"
#include "note-head.hh"
#include "musical-request.hh"
#include "tie.hh"
#include "translator-group.hh"

Tie_engraver::Tie_engraver()
{
  req_l_ = 0;
}


bool
Tie_engraver::do_try_music (Music *m)
{
  if (Tie_req * c = dynamic_cast<Tie_req*> (m))
    {
      req_l_ = c;
      SCM m = get_property ("automaticMelismata",0);
      bool am = gh_boolean_p (m) &&gh_scm2bool (m);
      if (am)
	{
	  set_melisma (true);
	}
      return true;
    }
  return false;
}

void
Tie_engraver::set_melisma (bool m)
{
  Translator_group *where = daddy_trans_l_;
  get_property ("tieMelismaBusy", &where);
  if (!where)
    where = daddy_trans_l_;
    
  daddy_trans_l_->set_property ("tieMelismaBusy", m ? SCM_BOOL_T : SCM_BOOL_F);
}

void
Tie_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_head *nh = dynamic_cast<Note_head *> (i.elem_l_))
    {
      Note_req * m = dynamic_cast<Note_req* > (i.req_l_);
      if (!m)
	return;
      now_heads_.push (CHead_melodic_tuple (nh, m, now_mom()+ m->length_mom ()));
    }
}

void
Tie_engraver::do_process_requests ()
{
  if (req_l_)
    {
      Moment now = now_mom ();
      Link_array<Note_head> nharr;
      
      stopped_heads_.clear ();
      while (past_notes_pq_.size ()
	     && past_notes_pq_.front ().end_ == now)
	stopped_heads_.push (past_notes_pq_.get ());
    }
}

void
Tie_engraver::process_acknowledged ()
{
  if (req_l_)
    {
      now_heads_.sort (CHead_melodic_tuple::pitch_compare);
      stopped_heads_.sort(CHead_melodic_tuple::pitch_compare);
      int i=0;
      int j=0;
      int tie_count=0;
      while  ( i < now_heads_.size () && j < stopped_heads_.size ())
	{
	  int comp
	    = Musical_pitch::compare (now_heads_[i].req_l_->pitch_ ,
				      stopped_heads_[j].req_l_->pitch_);

	  if (comp)
	    {
	      (comp < 0) ? i ++ : j++;
	      continue;
	    }
	  else
	    {
	      tie_count ++;

	      /* don't go around recreating ties that were already
		 made. Not infallible. Due to reordering in sort (),
		 we will make the wrong ties when noteheads are
		 added.  */
	      if (tie_count > tie_p_arr_.size ())
		{
		  Tie * p = new Tie;
		  p->set_head (LEFT, stopped_heads_[j].head_l_);
		  p->set_head (RIGHT, now_heads_[i].head_l_);
		  tie_p_arr_.push (p);
		  announce_element (Score_element_info (p, req_l_));
		}
	      i++;
	      j++;

	    }
	}

      if (!tie_p_arr_.size ())
	{
	  req_l_->warning (_ ("No ties were created!"));
	}
	  
    }
}


void
Tie_engraver::do_pre_move_processing ()
{
  for (int i=0; i < now_heads_.size (); i++)
    {
      past_notes_pq_.insert (now_heads_[i]);
    }
  now_heads_.clear ();

  SCM dir (get_property ("tieVerticalDirection", 0));
  SCM dir2 (get_property ("verticalDirection", 0));

  Direction tie_dir = CENTER;
  if (gh_number_p(dir))
    tie_dir = to_dir (dir);
  else if (isdir_b (dir2))
    tie_dir = to_dir (dir2);
  
  for (int i=0; i<  tie_p_arr_.size (); i++)
   {
      tie_p_arr_[i]->dir_ = tie_dir;
      typeset_element (tie_p_arr_[i]);
    }
  tie_p_arr_.clear ();
}

void
Tie_engraver::do_post_move_processing ()
{
  SCM m = get_property ("automaticMelismata",0);
  if (gh_boolean_p (m) && gh_scm2bool (m))
    {
      set_melisma (false);
    }
  req_l_ = 0;
  Moment now = now_mom ();
  while (past_notes_pq_.size () && past_notes_pq_.front ().end_ < now)
    past_notes_pq_.delmin ();
}

ADD_THIS_TRANSLATOR(Tie_engraver);


CHead_melodic_tuple::CHead_melodic_tuple ()
{
  head_l_ =0;
  req_l_ =0;
  end_ = 0;
}

CHead_melodic_tuple::CHead_melodic_tuple (Note_head *h, Melodic_req*m, Moment mom)
{
  head_l_ = h;
  req_l_ = m;
  end_ = mom;
}

int
CHead_melodic_tuple::pitch_compare (CHead_melodic_tuple const&h1,
			     CHead_melodic_tuple const &h2)
{
  return Melodic_req::compare (*h1.req_l_, *h2.req_l_);
}

int
CHead_melodic_tuple::time_compare (CHead_melodic_tuple const&h1,
			     CHead_melodic_tuple const &h2)
{
  return (h1.end_ - h2.end_ ).sign ();
}
