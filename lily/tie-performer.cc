/*   
  tie-performer.cc --  implement Tie_performer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2003 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */


#include "audio-item.hh"
#include "event.hh"
#include "pqueue.hh"
#include "performer.hh"

struct CNote_melodic_tuple {
  Music *event_ ;
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
  
  Array<CNote_melodic_tuple> now_notes_;
  Array<CNote_melodic_tuple> tied_notes_;

  Music *event_;
  Music * prev_event_;
  
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
  event_ = 0;
  done_ = false;
}

ENTER_DESCRIPTION (Tie_performer, "", "",
		   "tie-event",
		   "", "", "");


void
Tie_performer::initialize ()
{
  event_ = 0;
}


bool
Tie_performer::try_music (Music *m)
{
  if (!event_)
    {
      event_ = m;
      return true;
    }
  return false;
}

void
Tie_performer::acknowledge_audio_element (Audio_element_info i)
{
  if (Audio_note *nh = dynamic_cast<Audio_note *> (i.elem_))
    {
      Music *m = i.event_;
      if (m->is_mus_type ("note-event"))
	now_notes_.push (CNote_melodic_tuple (nh, m, now_mom ()+ m->get_length ()));
    }
}

void
Tie_performer::create_audio_elements ()
{
  /*
    This is a nested loop. Not optimal, but good enough.
   */
  if (tied_notes_.size ())
    {
      Moment now = now_mom();
      for (int i = tied_notes_.size (); i--; )
	{
	  if (tied_notes_[i].end_ != now)
	    continue;

	  for (int j = now_notes_.size(); j--;)
	    {
	      int comp
		= Pitch::compare (*unsmob_pitch (tied_notes_[i].event_->get_mus_property ("pitch")),
				  *unsmob_pitch (now_notes_[j].event_->get_mus_property ("pitch")));

	      if (comp == 0)
		{
		  
		  Audio_tie * p = new Audio_tie;
		  p->set_note (LEFT, tied_notes_[i].note_);
		  p->set_note (RIGHT, now_notes_[j].note_);
		  ties_.push (p);
		  announce_element (Audio_element_info (p, event_));

		  tied_notes_.del (i);
		}
	    }
	}
    }
}


void
Tie_performer::stop_translation_timestep ()
{
  if (prev_event_ && tied_notes_.size () && !ties_.size ())
    {
      prev_event_->origin ()->warning (_ ("No ties were performed."));
    }
  else
    prev_event_ = 0;
  
  if (event_)
    {
      tied_notes_ = now_notes_ ;
      prev_event_ = event_;
    }
  else
    {
      tied_notes_.clear (); 
    }

  event_ = 0;
  now_notes_ .clear ();

  for (int i=0; i<  ties_.size (); i++)
    {
      ties_[i]->note_drul_[RIGHT]->tie_to (ties_[i]->note_drul_[LEFT]);
    }
  
  ties_.clear ();
}

void
Tie_performer::start_translation_timestep ()
{
  event_ =0;
  done_ = false;
  Moment now = now_mom ();
  for (int i= tied_notes_.size ();
       i -- ;)
    {
      if (tied_notes_[i].end_ < now)
	tied_notes_.del (i);
      else
	break ;
    }
}


CNote_melodic_tuple::CNote_melodic_tuple ()
{
  note_ =0;
  event_ =0;
  end_ = 0;
}

CNote_melodic_tuple::CNote_melodic_tuple (Audio_note *h, Music*m, Moment mom)
{
  note_ = h;
  event_ = m;
  end_ = mom;
}

int
CNote_melodic_tuple::pitch_compare (CNote_melodic_tuple const&h1,
				    CNote_melodic_tuple const &h2)
{
  SCM p1  = h1.event_->get_mus_property ("pitch");
  SCM p2  = h2.event_->get_mus_property ("pitch");  
  return Pitch::compare (*unsmob_pitch (p1),
			       *unsmob_pitch (p2));
}

int
CNote_melodic_tuple::time_compare (CNote_melodic_tuple const&h1,
				   CNote_melodic_tuple const &h2)
{
  return (h1.end_ - h2.end_).main_part_.sign ();
}

