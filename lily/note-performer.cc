/*
  note-performer.cc -- implement Note_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2005 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "performer.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "warn.hh"

/**
Convert evs to audio notes.
*/
class Note_performer : public Performer {
public:
  TRANSLATOR_DECLARATIONS (Note_performer);
  
protected:
  virtual bool try_music (Music *ev) ;

  virtual void stop_translation_timestep ();
  virtual void create_audio_elements ();
 
private:
  Link_array<Music> note_evs_;
  Link_array<Audio_note> notes_;
  Link_array<Audio_note> delayeds_;
};

void 
Note_performer::create_audio_elements ()
{
  if (note_evs_.size ())
    {
      int transposing = 0;

      SCM prop = get_property ("instrumentTransposition");
      if (unsmob_pitch (prop)) 
	transposing = unsmob_pitch (prop)->semitone_pitch ();

      while (note_evs_.size ())
	{
	  Music* n = note_evs_.pop ();
	  SCM pit =  n->get_property ("pitch");

	  if (Pitch * pitp = unsmob_pitch (pit))
	    {
	      Audio_note* p = new Audio_note (*pitp,  n->get_length (),  - transposing);
	      Audio_element_info info (p, n);
	      announce_element (info);
	      notes_.push (p);
	    }
	}
      note_evs_.clear ();
    }
}


void
Note_performer::stop_translation_timestep ()
{
  // why don't grace notes show up here?
  // --> grace notes effectively do not get delayed
  Global_context * global = get_global_context ();
  for (int i = 0; i < notes_.size (); i++)
    {
      Audio_note* n = notes_[i];
      Moment m = n->delayed_until_mom_;
      if (m.to_bool ())
	{
	  global->add_moment_to_process (m);
	  delayeds_.push (n);
	  notes_[i] = 0;
	  notes_.del (i);
	  i--;
	}
    }

  Moment now = now_mom ();
  for (int i = 0; i < notes_.size (); i++)
    {
      play_element (notes_[i]);
    }
  notes_.clear ();
  note_evs_.clear ();
  for (int i = 0; i < delayeds_.size (); i++)
    {
      Audio_note* n = delayeds_[i];
      if (n->delayed_until_mom_ <= now)
	{
	  play_element (n);
	  delayeds_[i] = 0;
	  delayeds_.del (i);
	  i--;
	}
    }
}
 
bool
Note_performer::try_music (Music* ev)
{
  if (ev->is_mus_type ("note-event"))
    {
      note_evs_.push (ev);
      return true;
    }
  else if (ev->is_mus_type ("busy-playing-event"))
    return note_evs_.size ();
  
  return false;
}

ADD_TRANSLATOR (Note_performer,"","",
		  "note-event busy-playing-event","","","");

Note_performer::Note_performer ()
{
}
