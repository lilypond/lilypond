/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "event.hh"
#include "item.hh"
#include "context.hh"
#include "font-metric.hh"
#include "note-head.hh"

/**
   Generate texts for lyric syllables.  We only do one lyric at a time.  
   Multiple copies of this engraver should be used to do multiple voices.
 */
class Lyric_engraver : public Engraver 
{
protected:
  virtual void stop_translation_timestep ();
  virtual bool try_music (Music *);
  virtual void process_music ();
  
public:
  TRANSLATOR_DECLARATIONS (Lyric_engraver);
private:
  Music * event_;
  Item* text_;

  Context* get_voice_context ();
};

Lyric_engraver::Lyric_engraver ()
{
  text_ =0;
  event_ =0;
}

bool
Lyric_engraver::try_music (Music*r)
{
  if (r->is_mus_type ("lyric-event"))
    {
      if (event_)
	return false;
      event_ =r;
      return true;
    }
  return false;
}

void
Lyric_engraver::process_music ()
{
  if (event_)
    {
      text_=  make_item ("LyricText");
      
      text_->set_property ("text", event_->get_property ("text"));
      announce_grob (text_, event_->self_scm ());
    }
}


Context*
get_voice_to_lyrics (Context *lyrics)
{
  SCM avc = lyrics->get_property ("associatedVoiceContext");
  if  (Context *c = unsmob_context (avc))
    return c;

  SCM voice_name = lyrics->get_property ("associatedVoice");
  String nm = lyrics->id_string_;

  if (gh_string_p (voice_name))
    nm = ly_scm2string (voice_name);
  else
    {
      int idx = nm.index_last ('-');
      if (idx >= 0)
	nm = nm.left_string (idx);
    }

  Context *parent = lyrics;
  Context *voice = 0; 
  while (parent && !voice)
    {
      voice = parent->find_context_below (ly_symbol2scm ("Voice"), nm);
      parent = parent->daddy_context_;
    }

  if (voice)
    return voice;

  parent = lyrics;
  voice = 0; 
  while (parent && !voice)
    {
      voice = parent->find_context_below (ly_symbol2scm ("Voice"), "");
      parent = parent->daddy_context_;
    }

  return voice;
}
Grob *
get_current_note_head (Context * voice)
{
  for (SCM s = voice->get_property ("busyGrobs");
       gh_pair_p (s); s = gh_cdr (s))
    {
      Item*g = dynamic_cast<Item*> (unsmob_grob (gh_cdar (s)));
	  
      if (g && !g->get_column ()
	  && Note_head::has_interface (g))
	return g;
    }
	  
  return 0;  
}

void
Lyric_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      Context * voice = get_voice_to_lyrics (daddy_context_);

      if (voice)
	{
	  Grob *head = get_current_note_head (voice);

	  if (head)
	    {
	      text_->set_parent (head, X_AXIS);
	      if (melisma_busy (voice))
		text_->set_property ("self-alignment-X", gh_int2scm (LEFT)); 
	    }
	}
      
      typeset_grob (text_);
      text_ =0;
    }
  event_ =0;
}


ENTER_DESCRIPTION (Lyric_engraver,
/* descr */       "",
/* creats*/       "",
/* accepts */     "lyric-event",
/* acks  */      "",
/* reads */       "",
/* write */       "");
