/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

using std::string;

/**
   Generate texts for lyric syllables.  We only do one lyric at a time.
   Multiple copies of this engraver should be used to do multiple voices.
*/
class Lyric_engraver : public Engraver
{
protected:
  void stop_translation_timestep ();
  void process_music ();
  void listen_lyric (Stream_event *);

public:
  TRANSLATOR_DECLARATIONS (Lyric_engraver);

private:
  Stream_event *event_ = nullptr;
  Item *text_ = nullptr;
  Item *last_text_ = nullptr;

  Context *get_voice_context ();
};

Lyric_engraver::Lyric_engraver (Context *c)
  : Engraver (c)
{
}

void
Lyric_engraver::listen_lyric (Stream_event *ev)
{
  assign_event_once (event_, ev);
}

void
Lyric_engraver::process_music ()
{
  if (event_)
    {
      SCM text = get_property (event_, "text");

      if (ly_is_equal (text, scm_from_latin1_string (" ")))
        {
          if (last_text_)
            set_property (last_text_, "self-alignment-X",
                          get_property (this, "lyricMelismaAlignment"));
        }
      else
        text_ = make_item ("LyricText", event_->self_scm ());
    }

  Context *voice = get_voice_to_lyrics (context ());
  if (last_text_ && voice
      && from_scm<bool> (get_property (voice, "melismaBusy"))
      && !from_scm<bool> (get_property (context (), "ignoreMelismata")))
    set_property (last_text_, "self-alignment-X",
                  get_property (this, "lyricMelismaAlignment"));
}

Context *
get_voice_to_lyrics (Context *lyrics)
{
  bool searchForVoice
    = from_scm<bool> (get_property (lyrics, "searchForVoice"));

  SCM avc = get_property (lyrics, "associatedVoiceContext");
  if (Context *c = unsmob<Context> (avc))
    {
      if (!c->is_removable ())
        return c;
    }

  SCM voice_name = get_property (lyrics, "associatedVoice");
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

  SCM voice_type = get_property (lyrics, "associatedVoiceType");
  if (!scm_is_symbol (voice_type))
    return 0;

  Context *voice = find_context_near (lyrics, voice_type, nm);
  if (voice)
    return voice;

  return find_context_near (lyrics, voice_type, "");
}

Grob *
get_current_note_head (Context *voice)
{
  const auto now = voice->now_mom ();
  for (SCM s = get_property (voice, "busyGrobs"); scm_is_pair (s);
       s = scm_cdr (s))
    {
      Grob *g = unsmob<Grob> (scm_cdar (s));
      ;
      Moment *end_mom = unsmob<Moment> (scm_caar (s));
      if (!end_mom || !g)
        {
          programming_error ("busyGrobs invalid");
          continue;
        }

      // It's a bit irritating that we just have the length and
      // duration of the Grob.
      const auto end_from_now = now + get_event_length (g->event_cause (), now);
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
      if ((*end_mom == end_from_now) && dynamic_cast<Item *> (g)
          && has_interface<Note_head> (g))
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
              text_->set_x_parent (head->get_x_parent ());
              if (melisma_busy (voice)
                  && !from_scm<bool> (get_property (this, "ignoreMelismata")))
                set_property (text_, "self-alignment-X",
                              get_property (this, "lyricMelismaAlignment"));
            }
        }

      last_text_ = text_;
      text_ = nullptr;
    }
  event_ = nullptr;
}

void
Lyric_engraver::boot ()
{
  ADD_LISTENER (lyric);
}

ADD_TRANSLATOR (Lyric_engraver,
                /* doc */
                R"(
Engrave text for lyrics.
                )",

                /* create */
                R"(
LyricText
                )",

                /* read */
                R"(
ignoreMelismata
lyricMelismaAlignment
searchForVoice
                )",

                /* write */
                R"(

                )");
