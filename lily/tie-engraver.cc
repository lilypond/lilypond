/*   
  tie-engraver.cc --  implement Tie_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "command-request.hh"
#include "musical-request.hh"
#include "tie.hh"
#include "translator-group.hh"
#include "spanner.hh"
#include "tie-column.hh"
#include "engraver.hh"
#include "item.hh"
#include "grob-pitch-tuple.hh"
#include "warn.hh"
#include "note-head.hh"

/**
   Manufacture ties.  Acknowledge noteheads, and put them into a
   priority queue. If we have a Tie_req, connect the notes that finish
   just at this time, and note that start at this time.

   TODO: Remove the dependency on musical info. We should tie on the
   basis of position and duration-log of the heads (not of the reqs).

*/
class Tie_engraver : public Engraver
{
  Moment end_mom_;
  Moment next_end_mom_;

  Tie_req *req_l_;
  Link_array<Grob> now_heads_;
  Link_array<Grob> stopped_heads_;
  Link_array<Grob> tie_p_arr_;

  Spanner * tie_column_p_;
  
  void set_melisma (bool);
  
protected:
  virtual void start_translation_timestep ();
  virtual void stop_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
  virtual void create_grobs ();
  void typeset_tie (Grob*);
public:
  TRANSLATOR_DECLARATIONS(Tie_engraver);
};



Tie_engraver::Tie_engraver ()
{
  req_l_ = 0;
  tie_column_p_ = 0;
}


bool
Tie_engraver::try_music (Music *m)
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
Tie_engraver::acknowledge_grob (Grob_info i)
{
  if (Note_head::has_interface (i.grob_l_))
    {
      now_heads_.push (i.grob_l_);
    }
}

int
head_pitch_compare (Grob  *const&a,Grob  *const&b)
{
  Music *m1 =unsmob_music (a->get_grob_property ("cause"));
  Music *m2 =unsmob_music (b->get_grob_property ("cause"));  

  return Pitch::compare (* unsmob_pitch (m1->get_mus_property ("pitch")),
			 * unsmob_pitch (m2->get_mus_property ("pitch")));
}

void
Tie_engraver::create_grobs ()
{
  if (req_l_)
    {
      now_heads_.sort (&head_pitch_compare);
      /*
	We could sort stopped_heads_ as well (and use a linear alg. in
	stead of nested loop), but we'd have to use a stable sorting
	algorithm, since the ordering of the stopped heads (of the
	same pitch) is relevant.
       */

      SCM head_list = SCM_EOL;
      
      for (int i = now_heads_.size(); i--;)
	{
	  for (int j = stopped_heads_.size(); j--;)
	    {
	      int comp
		= head_pitch_compare (now_heads_[i], stopped_heads_[j]);

	      if (!comp)
		{
		  head_list  = gh_cons (gh_cons (stopped_heads_[j]->self_scm (),
						 now_heads_[i]->self_scm ()),
					head_list);

		  now_heads_.del (i);
		  stopped_heads_.del (j);
		  break ;
		}
	    }
	}
     
      SCM basic = get_property ("Tie");
      SCM sparse = get_property ("sparseTies");
      if (to_boolean (sparse))
	{
	  int i = scm_ilength (head_list);

	  if (!i)
	    return;
	  
	  SCM pair = scm_list_ref (head_list, gh_int2scm (i/2));
	  
	  Spanner * p = new Spanner (basic);

	  Tie::set_interface (p); // cannot remove.
	  Tie::set_head (p,LEFT, dynamic_cast<Item*> (unsmob_grob (ly_car (pair))));
	  Tie::set_head (p,RIGHT, dynamic_cast<Item*> (unsmob_grob (ly_cdr (pair))));
	  
	  tie_p_arr_.push (p);
	  announce_grob(p, req_l_->self_scm());
	}
      else for (SCM s = head_list; gh_pair_p (s); s = ly_cdr (s))
	{
	  Grob * p = new Spanner (basic);
	  Tie::set_interface (p); // cannot remove yet!
	  
	  Tie::set_head (p, LEFT, dynamic_cast<Item*> (unsmob_grob (ly_caar (s))));
	  Tie::set_head (p, RIGHT, dynamic_cast<Item*> (unsmob_grob (ly_cdar (s))));
	  
	  tie_p_arr_.push (p);
	  announce_grob(p, req_l_->self_scm());
	}

      if (tie_p_arr_.size () > 1 && !tie_column_p_)
	{
	  tie_column_p_ = new Spanner (get_property ("TieColumn"));

	  for (int i = tie_p_arr_.size (); i--;)
	    Tie_column::add_tie (tie_column_p_,tie_p_arr_ [i]);
	  announce_grob(tie_column_p_, SCM_EOL);
	}
    }
}


void
Tie_engraver::stop_translation_timestep ()
{
  req_l_ = 0;

  now_heads_.clear ();

  /*
    we don't warn for no ties, since this happens naturally when you
    use skipTypesetting.  */
  
  for (int i=0; i<  tie_p_arr_.size (); i++)
   {
      typeset_tie (tie_p_arr_[i]);
    }
  tie_p_arr_.clear ();
  if (tie_column_p_)
    {
      typeset_grob (tie_column_p_);
      tie_column_p_ =0;
    }
}

void
Tie_engraver::typeset_tie (Grob *her)
{
  if (! (Tie::head (her,LEFT) && Tie::head (her,RIGHT)))
    warning (_ ("lonely tie"));

  Direction d = LEFT;
  Drul_array<Grob *> new_head_drul;
  new_head_drul[LEFT] = Tie::head (her,LEFT);
  new_head_drul[RIGHT] = Tie::head (her,RIGHT);  
  do {
    if (!Tie::head (her,d))
      new_head_drul[d] = Tie::head (her, (Direction)-d);
  } while (flip (&d) != LEFT);

  index_set_cell (her->get_grob_property ("heads"), LEFT, new_head_drul[LEFT]->self_scm ());
  index_set_cell (her->get_grob_property ("heads"), RIGHT, new_head_drul[RIGHT]->self_scm ());

  typeset_grob (her);
}

void
Tie_engraver::start_translation_timestep ()
{
  SCM m = get_property ("automaticMelismata");
  if (to_boolean (m))
    {
      set_melisma (false);
    }

  SCM grobs = get_property ("busyGrobs");
  Moment now = now_mom();
  stopped_heads_.clear ();
  
  for (; gh_pair_p (grobs); grobs = gh_cdr (grobs))
    {
      Grob * grob  = unsmob_grob (gh_cdar (grobs));
      Moment end  =*unsmob_moment (gh_caar (grobs));
      
      /*
	This is slightly ugh: we are now confunding the frontend
	(iterators) and the backend (note heads) */
      if (end > now)
	break;
      else if (end == now
	       && Note_head::has_interface (grob))
	stopped_heads_.push (grob);
    }


  /*
    
    The list starts with entries that start earlier. By going through
    it, we reverse the order, where as we'd like to use the `last'
    heads first.

    This makes  a difference for grace notes. If we have

    c4 \grace c8 ~ c4

    Then busyGrobs will have ((1/4 . gc8) (1/4 . c4)). 

    We want stopped_heads_ to contain (c4 gc8), because we start with
    it at the top.
   */
  stopped_heads_.reverse();
}


ENTER_DESCRIPTION(Tie_engraver,
/* descr */       "Generate ties between noteheads of equal pitch.",
/* creats*/       "Tie TieColumn",
/* acks  */       "rhythmic-head-interface",
/* reads */       "sparseTies tieMelismaBusy",
/* write */       "");
