/*   
  tie-engraver.cc --  implement Tie_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "tie-engraver.hh"
#include "command-request.hh"
#include "rhythmic-head.hh"
#include "musical-request.hh"
#include "tie.hh"
#include "translator-group.hh"
#include "spanner.hh"
#include "tie-column.hh"
#include "pqueue.hh"
#include "engraver.hh"
#include "item.hh"

struct CHead_melodic_tuple {
  Melodic_req *req_l_ ;
  Score_element *head_l_;
  Moment end_;
  CHead_melodic_tuple ();
  CHead_melodic_tuple (Score_element*, Melodic_req*, Moment);
  static int pitch_compare (CHead_melodic_tuple const &, CHead_melodic_tuple const &);
  static int time_compare (CHead_melodic_tuple const &, CHead_melodic_tuple const &);  
};

inline int compare (CHead_melodic_tuple const &a, CHead_melodic_tuple const &b)
{
  return CHead_melodic_tuple::time_compare (a,b);
}


/**
   Manufacture ties.  Acknowledge noteheads, and put them into a
   priority queue. If we have a Tie_req, connect the notes that finish
   just at this time, and note that start at this time.

   TODO: junk the pq.
 */
class Tie_engraver : public Engraver
{
  PQueue<CHead_melodic_tuple> past_notes_pq_;
  Moment end_mom_;
  Moment next_end_mom_;

  Tie_req *req_l_;
  Array<CHead_melodic_tuple> now_heads_;
  Array<CHead_melodic_tuple> stopped_heads_;
  Link_array<Score_element> tie_p_arr_;

  Spanner * tie_column_p_;
  
  void set_melisma (bool);
  
protected:
  virtual void do_post_move_processing ();
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music*);
  virtual void do_process_music ();
  virtual void process_acknowledged ();
  void typeset_tie (Score_element*);
public:
  VIRTUAL_COPY_CONS(Translator);
  Tie_engraver();
};



Tie_engraver::Tie_engraver()
{
  req_l_ = 0;
  tie_column_p_ = 0;
}


bool
Tie_engraver::do_try_music (Music *m)
{
  if (Tie_req * c = dynamic_cast<Tie_req*> (m))
    {
      /*      if (end_mom_ > now_mom ())
       return false;
      */
      req_l_ = c;
      SCM m = get_property ("automaticMelismata");
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
  daddy_trans_l_->set_property ("tieMelismaBusy", m ? SCM_BOOL_T : SCM_BOOL_F);
}

void
Tie_engraver::acknowledge_element (Score_element_info i)
{
  if (Rhythmic_head::has_interface (i.elem_l_))
    {
      Note_req * m = dynamic_cast<Note_req* > (i.req_l_);
      if (!m)
	return;
      now_heads_.push (CHead_melodic_tuple (i.elem_l_, m, now_mom()+ m->length_mom ()));
    }
}

void
Tie_engraver::do_process_music ()
{
  if (req_l_)
    {
      Moment now = now_mom ();
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
	    = Musical_pitch::compare (*unsmob_pitch (now_heads_[i].req_l_->get_mus_property ("pitch") ),
				      *unsmob_pitch (stopped_heads_[j].req_l_->get_mus_property ("pitch")));

	  if (comp)
	    {
	      (comp < 0) ? j -- : i--;
	      continue;
	    }
	  else
	    {
	      head_list  = gh_cons (gh_cons (stopped_heads_[j].head_l_->self_scm (),
					     now_heads_[i].head_l_->self_scm ()),
				    head_list);

	      past_notes_pq_. insert (now_heads_[i]);
	      now_heads_.del (i);
	      stopped_heads_.del (j);
	      i--;
	      j--;
	    }
	}

      SCM basic = get_property ("Tie");
      SCM sparse = get_property ("sparseTies");
      if (to_boolean (sparse))
	{
	  int i = scm_ilength (head_list);

	  if (!i)
	    return;
	  
	  SCM pair = gh_list_ref (head_list, gh_int2scm (i/2));
	  
	  Spanner * p = new Spanner (basic);
	  Tie::set_head (p,LEFT, dynamic_cast<Item*> (unsmob_element (gh_car (pair))));
	  Tie::set_head (p,RIGHT, dynamic_cast<Item*> (unsmob_element (gh_cdr (pair))));
	  
	  tie_p_arr_.push (p);
	  announce_element (p, req_l_);
	}
      else for (SCM s = head_list; gh_pair_p (s); s = gh_cdr (s))
	{
	  Score_element * p = new Spanner (basic);
	  Tie::set_interface (p);
	  
	  Tie::set_head (p, LEFT, dynamic_cast<Item*> (unsmob_element (gh_caar (s))));
	  Tie::set_head (p, RIGHT, dynamic_cast<Item*> (unsmob_element (gh_cdar (s))));
	  
	  tie_p_arr_.push (p);
	  announce_element (p, req_l_);
	}

      if (!tie_p_arr_.size ())
	{
	  req_l_->origin ()->warning (_ ("No ties were created!"));
	}
      else if (tie_p_arr_.size () > 1 && !tie_column_p_)
	{
	  tie_column_p_ = new Spanner (get_property ("TieColumn"));
	  Tie_column::set_interface (tie_column_p_);
	  for (int i = tie_p_arr_.size (); i--; )
	    Tie_column::add_tie (tie_column_p_,tie_p_arr_ [i]);
	  announce_element (tie_column_p_, 0);
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
      typeset_tie (tie_p_arr_[i]);
    }
  tie_p_arr_.clear ();
  if (tie_column_p_)
    {
      typeset_element (tie_column_p_);
      tie_column_p_ =0;
    }
}

void
Tie_engraver::typeset_tie (Score_element *her)
{
  if (!(Tie::head (her,LEFT) && Tie::head (her,RIGHT)))
    warning (_ ("lonely tie"));

  Direction d = LEFT;
  Drul_array<Score_element *> new_head_drul;
  new_head_drul[LEFT] = Tie::head(her,LEFT);
  new_head_drul[RIGHT] = Tie::head (her,RIGHT);  
  do {
    if (!Tie::head (her,d))
      new_head_drul[d] = Tie::head(her,(Direction)-d);
  } while (flip(&d) != LEFT);

  index_set_cell (her->get_elt_property ("heads"), LEFT, new_head_drul[LEFT]->self_scm () );
  index_set_cell (her->get_elt_property ("heads"), RIGHT, new_head_drul[RIGHT]->self_scm () );

  typeset_element (her);
}

void
Tie_engraver::do_post_move_processing ()
{
  SCM m = get_property ("automaticMelismata");
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

CHead_melodic_tuple::CHead_melodic_tuple (Score_element *h, Melodic_req*m, Moment mom)
{
  head_l_ = h;
  req_l_ = m;
  end_ = mom;
}

int
CHead_melodic_tuple::pitch_compare (CHead_melodic_tuple const&h1,
			     CHead_melodic_tuple const &h2)
{
  SCM p1  = h1.req_l_->get_mus_property ("pitch");
  SCM p2  = h2.req_l_->get_mus_property ("pitch");
  
  return Musical_pitch::equal_p (p1,p2) == SCM_BOOL_T;
}

int
CHead_melodic_tuple::time_compare (CHead_melodic_tuple const&h1,
			     CHead_melodic_tuple const &h2)
{
  return (h1.end_ - h2.end_ ).sign ();
}
