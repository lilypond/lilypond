/*
  note-performer.cc -- implement Note_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2002 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "performer.hh"
#include "musical-request.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-translator.hh"
#include "warn.hh"

/**
Convert reqs to audio notes.
*/
class Note_performer : public Performer {
public:
  TRANSLATOR_DECLARATIONS(Note_performer);
  
protected:
  virtual bool try_music (Music *req) ;

  virtual void stop_translation_timestep ();
  virtual void create_audio_elements ();
  Global_translator* get_global_translator ();

private:
  Array<Note_req*> note_reqs_;
  Array<Audio_note*> notes_;
  Array<Audio_note*> delayeds_;
};

void 
Note_performer::create_audio_elements ()
{
  if (note_reqs_.size ())
    {
      int transposing_i = 0;
      //urg
      SCM prop = get_property ("transposing");
      if (gh_number_p (prop)) 
	transposing_i = gh_scm2int (prop);

      while (note_reqs_.size ())
	{
	  Note_req* n = note_reqs_.pop ();
	  Pitch pit =  * unsmob_pitch (n->get_mus_property ("pitch"));
	  Audio_note* p = new Audio_note (pit,  n->length_mom (), transposing_i);
	  Audio_element_info info (p, n);
	  announce_element (info);
	  notes_.push (p);
	}
      note_reqs_.clear ();
    }
}

Global_translator*
Note_performer::get_global_translator ()
{
  Translator *t = this;
  Global_translator *global =0;
  do
    {
      t = t->daddy_trans_ ;
      global = dynamic_cast<Global_translator*> (t);
    }
  while (!global);

  return global;
}


void
Note_performer::stop_translation_timestep ()
{

  // why don't grace notes show up here?
  // --> grace notes effectively do not get delayed
  Global_translator* global = get_global_translator ();
  for (int i=0; i < notes_.size (); i++)
    {
      Audio_note* n = notes_[i];
      Moment m= n->delayed_until_mom_;
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
  for (int i=0; i < notes_.size (); i++)
    {
      play_element (notes_[i]);
    }
  notes_.clear ();
  note_reqs_.clear ();
  for (int i=0; i < delayeds_.size (); i++)
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
Note_performer::try_music (Music* req)
{
  if (Note_req *nr = dynamic_cast <Note_req *> (req))
    {
      note_reqs_.push (nr);
      return true;
    }
  return false;
}

ENTER_DESCRIPTION(Note_performer,"","","","","");

Note_performer::Note_performer()
{
}
