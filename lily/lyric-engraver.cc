/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2014 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "context.hh"
#include "engraver.hh"
#include "item.hh"
#include "note-head.hh"
#include "stream-event.hh"
#include "international.hh"

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

      if (ly_is_equal (text, scm_from_locale_string (" ")))
        {
          if (last_text_)
            last_text_->set_property ("self-alignment-X",
                                      get_property ("lyricMelismaAlignment"));
        }
      else
        text_ = make_item ("LyricText", event_->self_scm ());
    }

  Context *voice = get_voice_to_lyrics (context ());
  if (last_text_
      && voice
      && to_boolean (voice->get_property ("melismaBusy"))
      && !to_boolean (context ()->get_property ("ignoreMelismata")))
    last_text_->set_property ("self-alignment-X",
                              get_property ("lyricMelismaAlignment"));
}

Context *
get_voice_to_lyrics (Context *lyrics)
{
  bool searchForVoice = to_boolean (lyrics->get_property ("searchForVoice"));

  SCM avc = lyrics->get_property ("associatedVoiceContext");
  if (Context *c = Context::unsmob (avc))
    return c;

  SCM voice_name = lyrics->get_property ("associatedVoice");
  string nm = lyrics->id_string ();

  if (scm_is_string (voice_name))
    nm = ly_scm2string (voice_name);
  else if (nm == "" || !searchForVoice)
    return 0;
  else
    {
      ssize idx = nm.rfind ('-');
      if (idx != NPOS)
        nm = nm.substr (0, idx);
    }

  SCM voice_type = lyrics->get_property ("associatedVoiceType");
  if (!scm_is_symbol (voice_type))
    return 0;

  Context *parent = lyrics;
  Context *voice = 0;
  while (parent && !voice)
    {
      voice = find_context_below (parent, voice_type, nm);
      parent = parent->get_parent_context ();
    }

  if (voice)
    return voice;

  parent = lyrics;
  voice = 0;
  while (parent && !voice)
    {
      voice = find_context_below (parent, voice_type, "");
      parent = parent->get_parent_context ();
    }

  return voice;
}

Grob *
get_current_note_head (Context *voice)
{
  Moment now = voice->now_mom ();
  for (SCM s = voice->get_property ("busyGrobs");
       scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *g = Grob::unsmob (scm_cdar (s));;
      Moment *end_mom = Moment::unsmob (scm_caar (s));
      if (!end_mom || !g)
        {
          programming_error ("busyGrobs invalid");
          continue;
        }

      // It's a bit irritating that we just have the length and
      // duration of the Grob.
      Moment end_from_now =
        get_event_length (Stream_event::unsmob (g->get_property ("cause")), now)
        + now;
      // We cannot actually include more than a single grace note
      // using busyGrobs on ungraced lyrics since a grob ending on
      // grace time will just have disappeared from busyGrobs by the
      // time our ungraced lyrics appear.  At best we may catch a
      // single grace note.
      //
      // However, a single grace note ending on a non-grace time is
      // indistinguishable from a proper note ending on a non-grace
      // time.  So we really have no way to obey includeGraceNotes
      // here.  Not with this mechanism.
      if ((*end_mom == end_from_now)
          && dynamic_cast<Item *> (g)
          && Note_head::has_interface (g))
        {
          return g;
        }
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
              text_->set_parent (head->get_parent(X_AXIS), X_AXIS);
              if (melisma_busy (voice)
                  && !to_boolean (get_property ("ignoreMelismata")))
                text_->set_property ("self-alignment-X",
                                     get_property ("lyricMelismaAlignment"));
            }
        }

      last_text_ = text_;
      text_ = 0;
    }
  event_ = 0;
}

ADD_TRANSLATOR (Lyric_engraver,
                /* doc */
                "Engrave text for lyrics.",

                /* create */
                "LyricText ",

                /* read */
                "ignoreMelismata "
                "lyricMelismaAlignment "
                "searchForVoice",

                /* write */
                ""
               );
