/*   
  new-tie-engraver.cc --  implement Tie_performer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "context.hh"
#include "audio-item.hh"
#include "event.hh"
#include "pqueue.hh"
#include "performer.hh"

class Tie_performer : public Performer
{
  Music *event_;
  Music *last_event_;
  Array<Audio_element_info> now_heads_;
  Array<Audio_element_info> heads_to_tie_;

  bool ties_created_;
  
protected:
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void acknowledge_audio_element (Audio_element_info);
  virtual bool try_music (Music*);
  virtual void process_music ();
public:
  TRANSLATOR_DECLARATIONS (Tie_performer);
};



Tie_performer::Tie_performer ()
{
  event_ = 0;
  last_event_  = 0;
  ties_created_ = false;
}


bool
Tie_performer::try_music (Music *mus)
{
  if (mus->is_mus_type ("tie-event"))
    {
      event_ = mus;
    }
  
  return true;
}

void
Tie_performer::process_music ()
{
  if (event_)
    daddy_context_->set_property ("tieMelismaBusy", SCM_BOOL_T);
}

void
Tie_performer::acknowledge_audio_element (Audio_element_info inf)
{
  if (Audio_note * an = dynamic_cast<Audio_note *> (inf.elem_))
    {
      now_heads_.push (inf);
      for  (int i = heads_to_tie_.size (); i--;)
	{
	  Music * right_mus = inf.event_;
	  
	  Audio_note *th =  dynamic_cast<Audio_note*> (heads_to_tie_[i].elem_);
	  Music * left_mus = heads_to_tie_[i].event_;

	  if (right_mus && left_mus
	      && gh_equal_p (right_mus->get_property ("pitch"),
			     left_mus->get_property ("pitch")))
	    {
	      an->tie_to (th);
	      ties_created_ = true;  
	    }
	}
    }
}

void
Tie_performer::start_translation_timestep ()
{
  daddy_context_->set_property ("tieMelismaBusy",
			      gh_bool2scm (heads_to_tie_.size ()));
      
}

void
Tie_performer::stop_translation_timestep ()
{
  if (ties_created_)
    {
      heads_to_tie_.clear ();
      last_event_ = 0;
      ties_created_ = false;
    }
  
  if (event_)
    {
      heads_to_tie_ = now_heads_;
      last_event_ = event_;
    }
  event_ = 0;
  now_heads_.clear ();
}

ENTER_DESCRIPTION (Tie_performer,
/* descr */       "Generate ties between noteheads of equal pitch.",
/* creats*/       "",
/* accepts */     "tie-event",
/* acks  */       "",
/* reads */       "tieMelismaBusy",
/* write */       "");
