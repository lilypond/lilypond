/*   
  tie-performer.cc --  implement Tie_performer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "command-request.hh"
#include "audio-item.hh"
#include "musical-request.hh"
#include "pqueue.hh"
#include "performer.hh"

struct CNote_melodic_tuple {
  Melodic_req *req_l_ ;
  Audio_note *note_l_;
  Moment end_;
  CNote_melodic_tuple ();
  CNote_melodic_tuple (Audio_note*, Melodic_req*, Moment);
  static int pitch_compare (CNote_melodic_tuple const &, CNote_melodic_tuple const &);
  static int time_compare (CNote_melodic_tuple const &, CNote_melodic_tuple const &);  
};

inline int compare (CNote_melodic_tuple const &a, CNote_melodic_tuple const &b)
{
  return CNote_melodic_tuple::time_compare (a,b);
}


/**
   Manufacture ties.  Acknowledge notes, and put them into a
   priority queue. If we have a Tie_req, connect the notes that finish
   just at this time, and note that start at this time.

   TODO: should share code with Tie_engraver ?
 */
class Tie_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Tie_performer ();
private:
  bool done_;
  PQueue<CNote_melodic_tuple> past_notes_pq_;
  Tie_req *req_l_;
  Array<CNote_melodic_tuple> now_notes_;
  Array<CNote_melodic_tuple> stopped_notes_;
  Link_array<Audio_tie> tie_p_arr_;
  
protected:
  virtual void initialize ();
  virtual void start_translation_timestep ();
  virtual void stop_translation_timestep ();
  virtual void acknowledge_audio_element (Audio_element_info);
  virtual bool try_music (Music*);
  virtual void create_audio_elements ();
};


Tie_performer::Tie_performer ()
{
  req_l_ = 0;
  done_ = false;
}

ADD_THIS_TRANSLATOR (Tie_performer);


#if 0
Tie_performer::Tie_performer ()
{
  // URG
  // if we don't do this, lily dumps core
  // which means that ``initialize'' and
  // ``start_translation_timestep'' did not happen?!
  initialize ();
}
#endif

void
Tie_performer::initialize ()
{
  req_l_ = 0;
}


bool
Tie_performer::try_music (Music *m)
{
  if (!req_l_)
    {
      if (Tie_req * c = dynamic_cast<Tie_req*> (m))
	{
	  req_l_ = c;
	  return true;
	}
    }
  return false;
}

void
Tie_performer::acknowledge_audio_element (Audio_element_info i)
{
  if (Audio_note *nh = dynamic_cast<Audio_note *> (i.elem_l_))
    {
      Note_req * m = dynamic_cast<Note_req* > (i.req_l_);
      if (!m)
	return;
      now_notes_.push (CNote_melodic_tuple (nh, m, now_mom ()+ m->length_mom ()));
    }
}

void
Tie_performer::create_audio_elements ()
{
  if (req_l_ && ! done_)
    {
      Moment now = now_mom ();
      Link_array<Audio_note> nharr;
      
      stopped_notes_.clear ();
      while (past_notes_pq_.size ()
	     && past_notes_pq_.front ().end_ == now)
	stopped_notes_.push (past_notes_pq_.get ());
      done_ = true;
      return;
    }

  if (req_l_)
    {
      now_notes_.sort (CNote_melodic_tuple::pitch_compare);
      stopped_notes_.sort (CNote_melodic_tuple::pitch_compare);
      int i=0;
      int j=0;
      int tie_count=0;
      while (i < now_notes_.size () && j < stopped_notes_.size ())
	{
	  int comp
	    = Pitch::compare (*unsmob_pitch (now_notes_[i].req_l_->get_mus_property ("pitch")),
				      *unsmob_pitch (stopped_notes_[j].req_l_->get_mus_property ("pitch")));

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
		 we will make the wrong ties when notenotes are
		 added.  */
	      if (tie_count > tie_p_arr_.size ())
		{
		  Audio_tie * p = new Audio_tie;
		  p->set_note (LEFT, stopped_notes_[j].note_l_);
		  p->set_note (RIGHT, now_notes_[i].note_l_);
		  tie_p_arr_.push (p);
		      announce_element (Audio_element_info (p, req_l_));
		}
	      i++;
	      j++;
	      
	    }
	}
      
      if (!tie_p_arr_.size ())
	{
	  req_l_->origin ()->warning (_ ("No ties were created!"));
	}
    }
}

void
Tie_performer::stop_translation_timestep ()
{
  for (int i=0; i < now_notes_.size (); i++)
    {
      past_notes_pq_.insert (now_notes_[i]);
    }
  now_notes_.clear ();

  for (int i=0; i<  tie_p_arr_.size (); i++)
   {
     //play_element (tie_p_arr_[i]);
     tie_p_arr_[i]->note_l_drul_[RIGHT]->tie_to (tie_p_arr_[i]->note_l_drul_[LEFT]);
   }
  tie_p_arr_.clear ();
}

void
Tie_performer::start_translation_timestep ()
{
  req_l_ =0;
  done_ = false;
  Moment now = now_mom ();
  while (past_notes_pq_.size () && past_notes_pq_.front ().end_ < now)
    past_notes_pq_.delmin ();
}


CNote_melodic_tuple::CNote_melodic_tuple ()
{
  note_l_ =0;
  req_l_ =0;
  end_ = 0;
}

CNote_melodic_tuple::CNote_melodic_tuple (Audio_note *h, Melodic_req*m, Moment mom)
{
  note_l_ = h;
  req_l_ = m;
  end_ = mom;
}

int
CNote_melodic_tuple::pitch_compare (CNote_melodic_tuple const&h1,
				    CNote_melodic_tuple const &h2)
{
  SCM p1  = h1.req_l_->get_mus_property ("pitch");
  SCM p2  = h2.req_l_->get_mus_property ("pitch");  
  return Pitch::compare (*unsmob_pitch (p1),
			       *unsmob_pitch (p2));
}

int
CNote_melodic_tuple::time_compare (CNote_melodic_tuple const&h1,
				   CNote_melodic_tuple const &h2)
{
  return (h1.end_ - h2.end_).main_part_.sign ();
}
