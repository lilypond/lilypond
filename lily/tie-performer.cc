/*   
  tie-performer.cc --  implement Tie_performer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */


#include "audio-item.hh"
#include "request.hh"
#include "pqueue.hh"
#include "performer.hh"

struct CNote_melodic_tuple {
  Music *req_ ;
  Audio_note *note_;
  Moment end_;
  CNote_melodic_tuple ();
  CNote_melodic_tuple (Audio_note*, Music*, Moment);
  static int pitch_compare (CNote_melodic_tuple const &, CNote_melodic_tuple const &);
  static int time_compare (CNote_melodic_tuple const &, CNote_melodic_tuple const &);  
};

inline int compare (CNote_melodic_tuple const &a, CNote_melodic_tuple const &b)
{
  return CNote_melodic_tuple::time_compare (a,b);
}


/**
   Manufacture ties.  Acknowledge notes, and put them into a
   priority queue. If we have a Music, connect the notes that finish
   just at this time, and note that start at this time.

   TODO: should share code with Tie_engraver ?
 */
class Tie_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS(Tie_performer);
private:
  bool done_;
  PQueue<CNote_melodic_tuple> past_notes_pq_;
  Music *req_;
  Array<CNote_melodic_tuple> now_notes_;
  Array<CNote_melodic_tuple> stopped_notes_;
  Link_array<Audio_tie> ties_;
  
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
  req_ = 0;
  done_ = false;
}

ENTER_DESCRIPTION (Tie_performer, "", "",
		   "tie-event",
		   "", "", "");


void
Tie_performer::initialize ()
{
  req_ = 0;
}


bool
Tie_performer::try_music (Music *m)
{
  if (!req_)
    {
      req_ = m;
      return true;
    }
  return false;
}

void
Tie_performer::acknowledge_audio_element (Audio_element_info i)
{
  if (Audio_note *nh = dynamic_cast<Audio_note *> (i.elem_))
    {
      Music *m = i.req_;
      if (m->is_mus_type ("note-event"))
	now_notes_.push (CNote_melodic_tuple (nh, m, now_mom ()+ m->length_mom ()));
    }
}

void
Tie_performer::create_audio_elements ()
{
  if (req_ && ! done_)
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

  if (req_)
    {
      now_notes_.sort (CNote_melodic_tuple::pitch_compare);
      stopped_notes_.sort (CNote_melodic_tuple::pitch_compare);
      int i=0;
      int j=0;
      int tie_count=0;
      while (i < now_notes_.size () && j < stopped_notes_.size ())
	{
	  int comp
	    = Pitch::compare (*unsmob_pitch (now_notes_[i].req_->get_mus_property ("pitch")),
				      *unsmob_pitch (stopped_notes_[j].req_->get_mus_property ("pitch")));

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
	      if (tie_count > ties_.size ())
		{
		  Audio_tie * p = new Audio_tie;
		  p->set_note (LEFT, stopped_notes_[j].note_);
		  p->set_note (RIGHT, now_notes_[i].note_);
		  ties_.push (p);
		      announce_element (Audio_element_info (p, req_));
		}
	      i++;
	      j++;
	      
	    }
	}
      
      if (!ties_.size ())
	{
	  req_->origin ()->warning (_ ("No ties were created!"));
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

  for (int i=0; i<  ties_.size (); i++)
   {
     //play_element (ties_[i]);
     ties_[i]->note_l_drul_[RIGHT]->tie_to (ties_[i]->note_l_drul_[LEFT]);
   }
  ties_.clear ();
}

void
Tie_performer::start_translation_timestep ()
{
  req_ =0;
  done_ = false;
  Moment now = now_mom ();
  while (past_notes_pq_.size () && past_notes_pq_.front ().end_ < now)
    past_notes_pq_.delmin ();
}


CNote_melodic_tuple::CNote_melodic_tuple ()
{
  note_ =0;
  req_ =0;
  end_ = 0;
}

CNote_melodic_tuple::CNote_melodic_tuple (Audio_note *h, Music*m, Moment mom)
{
  note_ = h;
  req_ = m;
  end_ = mom;
}

int
CNote_melodic_tuple::pitch_compare (CNote_melodic_tuple const&h1,
				    CNote_melodic_tuple const &h2)
{
  SCM p1  = h1.req_->get_mus_property ("pitch");
  SCM p2  = h2.req_->get_mus_property ("pitch");  
  return Pitch::compare (*unsmob_pitch (p1),
			       *unsmob_pitch (p2));
}

int
CNote_melodic_tuple::time_compare (CNote_melodic_tuple const&h1,
				   CNote_melodic_tuple const &h2)
{
  return (h1.end_ - h2.end_).main_part_.sign ();
}

