/*   
  ctie-engraver.cc --  implement Tie_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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

      SCM head_list = SCM_EOL;
      
      int j = stopped_heads_.size ()-1;
      int i = now_heads_.size ()-1;

      while  (i >= 0 && j >=0)
	{
	  int comp
	    = Musical_pitch::compare (now_heads_[i].req_l_->pitch_ ,
				      stopped_heads_[j].req_l_->pitch_);

	  if (comp)
	    {
	      (comp < 0) ? j -- : i--;
	      continue;
	    }
	  else
	    {
	      head_list  = gh_cons (gh_cons (stopped_heads_[j].head_l_->self_scm_,
					     now_heads_[i].head_l_->self_scm_),
				    head_list);

	      past_notes_pq_. insert (now_heads_[i]);
	      now_heads_.del (i);
	      stopped_heads_.del (j);
	      i--;
	      j--;
	    }
	}


      SCM sparse = get_property ("sparseTies", 0);
      if (to_boolean (sparse))
	{
	  int i = scm_ilength (head_list);

	  if (!i)
	    return;
	  
	  SCM pair = gh_list_ref (head_list, gh_int2scm (i/2));
	  
	  Tie * p = new Tie;
	  p->set_head (LEFT, dynamic_cast<Item*> (unsmob_element (gh_car (pair))));
	  p->set_head (RIGHT, dynamic_cast<Item*> (unsmob_element (gh_cdr (pair))));
	  
	  tie_p_arr_.push (p);
	  announce_element (Score_element_info (p, req_l_));
	}
      else for (SCM s = head_list; gh_pair_p (s); s = gh_cdr (s))
	{
	  Tie * p = new Tie;
	  p->set_head (LEFT, dynamic_cast<Item*> (unsmob_element (gh_caar (s))));
	  p->set_head (RIGHT, dynamic_cast<Item*> (unsmob_element (gh_cdar (s))));
	  
	  tie_p_arr_.push (p);
	  announce_element (Score_element_info (p, req_l_));
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

  for (int i=0; i<  tie_p_arr_.size (); i++)
   {
      typeset_element (tie_p_arr_[i]);
    }
  tie_p_arr_.clear ();
}

void
Tie_engraver::do_post_move_processing ()
{
  SCM m = get_property ("automaticMelismata",0);
  if (to_boolean (m))
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
