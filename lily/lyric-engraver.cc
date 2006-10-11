/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "context.hh"
#include "engraver.hh"
#include "font-metric.hh"
#include "item.hh"
#include "multi-measure-rest.hh"
#include "note-head.hh"
#include "rest.hh"
#include "stream-event.hh"

#include "translator.icc"

/**
   Generate texts for lyric syllables.  We only do one lyric at a time.
   Multiple copies of this engraver should be used to do multiple voices.
*/
class Lyric_engraver : public Engraver
{
protected:
  void stop_translation_timestep ();
  void process_music ();
  DECLARE_TRANSLATOR_LISTENER (lyric);

public:
  TRANSLATOR_DECLARATIONS (Lyric_engraver);

private:
  Stream_event *event_;
  Item *text_;
  Item *last_text_;

  Context *get_voice_context ();
};

Lyric_engraver::Lyric_engraver ()
{
  text_ = 0;
  last_text_ = 0;
  event_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Lyric_engraver, lyric);
void
Lyric_engraver::listen_lyric (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (event_, ev);
}

void
Lyric_engraver::process_music ()
{
  if (event_)
    {
      SCM text = event_->get_property ("text");

      if (ly_is_equal (text, scm_makfrom0str (" ")))
	{
	  if (last_text_)
	    last_text_->set_property ("self-alignment-X", scm_from_int (LEFT));
	}
      else
	{
	  text_ = make_item ("LyricText", event_->self_scm ());
	}
    }
}

Context *
get_voice_to_lyrics (Context *lyrics)
{
  SCM avc = lyrics->get_property ("associatedVoiceContext");
  if (Context *c = unsmob_context (avc))
    return c;

  SCM voice_name = lyrics->get_property ("associatedVoice");
  string nm = lyrics->id_string ();

  if (scm_is_string (voice_name))
    nm = ly_scm2string (voice_name);
  else
    {
      ssize idx = nm.rfind ('-');
      if (idx != NPOS)
	nm = nm.substr (0, idx);
    }

  Context *parent = lyrics;
  Context *voice = 0;
  while (parent && !voice)
    {
      voice = find_context_below (parent, ly_symbol2scm ("Voice"), nm);
      parent = parent->get_parent_context ();
    }

  if (voice)
    return voice;

  parent = lyrics;
  voice = 0;
  while (parent && !voice)
    {
      voice = find_context_below (parent, ly_symbol2scm ("Voice"), "");
      parent = parent->get_parent_context ();
    }

  return voice;
}

Grob *
get_current_note_head (Context *voice)
{
  for (SCM s = voice->get_property ("busyGrobs");
       scm_is_pair (s); s = scm_cdr (s))
    {
      Item *g = dynamic_cast<Item *> (unsmob_grob (scm_cdar (s)));

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
      Context *voice = get_voice_to_lyrics (context ());

      if (voice)
	{
	  Grob *head = get_current_note_head (voice);

	  if (head)
	    {
	      text_->set_parent (head, X_AXIS);
	      if (melisma_busy (voice))
		text_->set_property ("self-alignment-X", scm_from_int (LEFT));
	    }
	}

      last_text_ = text_;
      text_ = 0;
    }
  event_ = 0;
}

ADD_TRANSLATOR (Lyric_engraver,
		/* doc */ "",
		/* create */ "LyricText",
		/* accept */ "lyric-event",
		/* read */ "",
		/* write */ "");
