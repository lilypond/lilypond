/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2023 Glen Prideaux <glenprideaux@iname.com>,
  Han-Wen Nienhuys <hanwen@xs4all.nl>,
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
#include "international.hh"
#include "item.hh"
#include "lyric-extender.hh"
#include "note-head.hh"
#include "pointer-group-interface.hh"
#include "stream-event.hh"
#include "warn.hh"
#include "spanner.hh"
#include "translator.icc"

void completize_extender (Spanner *sp);

class Extender_engraver final : public Engraver
{
  Stream_event *extender_ev_ = nullptr;
  Stream_event *lyric_ev_ = nullptr;
  Stream_event *hyphen_ev_ = nullptr;
  Spanner *extender_ = nullptr;
  Spanner *pending_extender_ = nullptr;

  bool pending_autoextender_ = false;
  bool pending_autoextender_melisma_busy_ = false;

public:
  TRANSLATOR_DECLARATIONS (Extender_engraver);

protected:
  void listen_extender (Stream_event *);
  void listen_completize_extender (Stream_event *);
  void listen_lyric (Stream_event *);
  void listen_hyphen (Stream_event *);
  void acknowledge_lyric_syllable (Grob_info_t<Item>);

  void finalize () override;

  void stop_translation_timestep ();
  void process_music ();
};

Extender_engraver::Extender_engraver (Context *c)
  : Engraver (c)
{
}

void
Extender_engraver::listen_lyric (Stream_event *ev)
{
  // Do not use assign_event_once - we let the listener in lyric-engraver.cc
  // warn about conflicting events instead.
  if (!lyric_ev_)
    lyric_ev_ = ev;

  SCM text = get_property (lyric_ev_, "text");

  if (ly_is_equal (text, to_scm (" ")))
    {
      // Don't register _ as a lyric starting an autoextender ...
      lyric_ev_ = nullptr;

      // ... but note that we actually are inside a melisma now.
      if (pending_autoextender_)
        pending_autoextender_melisma_busy_ = true;
    }
  else if (pending_extender_ && pending_autoextender_
           && !pending_autoextender_melisma_busy_)
    {
      pending_extender_->suicide ();
      pending_extender_ = nullptr;
    }
}

void
Extender_engraver::listen_hyphen (Stream_event *ev)
{
  // Do not use assign_event_once - we let the listener in hyphen-engraver.cc
  // warn about conflicting events instead.
  if (!hyphen_ev_)
    hyphen_ev_ = ev;
}

void
Extender_engraver::listen_extender (Stream_event *ev)
{
  assign_event_once (extender_ev_, ev);
}
/*
  A CompletizeExtenderEvent is sent at the end of each lyrics block
  to ensure any pending extender can be correctly terminated if the lyrics
  end before the associated voice (this prevents the right bound being extended
  to the next note-column if no lyric follows the extender)
*/
void
Extender_engraver::listen_completize_extender (Stream_event * /* ev */)
{
  if (pending_extender_)
    {
      completize_extender (pending_extender_);
      pending_extender_ = nullptr;
    }
}

void
Extender_engraver::process_music ()
{
  bool create_extender = false;
  Stream_event *causing_event = nullptr;

  if (extender_ev_)
    {
      create_extender = true;
      causing_event = extender_ev_;
      pending_autoextender_ = false;
    }
  else if (lyric_ev_ && !hyphen_ev_
           && from_scm<bool> (get_property (this, "autoExtenders")))
    {
      Context *voice = get_voice_to_lyrics (context ());

      if (voice)
        {
          /*
        We'd basically like to create an auto-extender only if the
        voice is in a melisma.  But melismata indicated by _ in lyric
        mode should also work, and these can be detected only when
        the *next* lyric event comes along.  So we create an
        auto-extender unconditionally and kill it off when the next
        lyric event comes along if that isn't an underscore _.
      */
          create_extender = true;
          causing_event = lyric_ev_;
          pending_autoextender_ = true;

          // clean up pending_autoextender_melisma_busy_ from inherited state;
          // this boolean will be set to true, if necessary, in
          // stop_translation_timestep or even (in case of a Lyric melisma _)
          // in a future timestep.
          pending_autoextender_melisma_busy_ = false;
        }
    }

  if (create_extender)
    {
      extender_ = make_spanner ("LyricExtender", causing_event->self_scm ());
      set_property (extender_, "auto-generated",
                    to_scm (pending_autoextender_));
    }
}

void
Extender_engraver::acknowledge_lyric_syllable (Grob_info_t<Item> info)
{
  auto *const item = info.grob ();
  if (extender_)
    extender_->set_bound (LEFT, item);

  if (pending_extender_)
    {
      set_object (pending_extender_, "next", item->self_scm ());
      completize_extender (pending_extender_);
      pending_extender_ = nullptr;
    }
}

void
Extender_engraver::stop_translation_timestep ()
{
  Context *voice = get_voice_to_lyrics (context ());

  if (voice && pending_autoextender_)
    {
      // delay call to melisma_busy () until here since a melisma might have
      // started during timestep's processing
      pending_autoextender_melisma_busy_
        = pending_autoextender_melisma_busy_ || melisma_busy (voice);
    }

  if (extender_ || pending_extender_)
    {
      Grob *h = voice ? get_current_note_head (voice) : 0;

      if (h)
        {
          if (extender_)
            {
              Pointer_group_interface::add_grob (extender_,
                                                 ly_symbol2scm ("heads"), h);
            }

          if (pending_extender_)
            {
              Pointer_group_interface::add_grob (pending_extender_,
                                                 ly_symbol2scm ("heads"), h);
            }
        }
      else
        {
          if (pending_extender_
              && !from_scm<bool> (get_property (this, "extendersOverRests")))
            {
              completize_extender (pending_extender_);
              pending_extender_ = nullptr;
            }
        }
      if (extender_)
        {
          // We guarantee that after stop_translation_timestep,
          // extender_ is always null.
          pending_extender_ = extender_;
          extender_ = nullptr;
        }
    }

  extender_ev_ = nullptr;
  lyric_ev_ = nullptr;
  hyphen_ev_ = nullptr;
}

void
completize_extender (Spanner *sp)
{
  if (!sp->get_bound (RIGHT))
    {
      extract_grob_set (sp, "heads", heads);
      if (!heads.empty ())
        {
          // extract_item_set () would clean this up, but would be wasteful
          // given that we need to use only one of the elements
          if (auto *const head = dynamic_cast<Item *> (heads.back ()))
            sp->set_bound (RIGHT, head);
          else
            heads.back ()->programming_error ("non-item among heads");
        }
    }
}

void
Extender_engraver::finalize ()
{
  if (pending_extender_)
    {
      if (pending_autoextender_ && !pending_autoextender_melisma_busy_)
        {
          pending_extender_->suicide ();
        }
      else
        {
          completize_extender (pending_extender_);

          if (!pending_extender_->get_bound (RIGHT))
            pending_extender_->warning (_ ("unterminated extender"));
        }

      pending_extender_ = nullptr;
    }
}

void
Extender_engraver::boot ()
{
  ADD_LISTENER (lyric);
  ADD_LISTENER (hyphen);
  ADD_LISTENER (extender);
  ADD_LISTENER (completize_extender);
  ADD_ACKNOWLEDGER (lyric_syllable);
}

ADD_TRANSLATOR (Extender_engraver,
                /* doc */
                R"(
Create lyric extenders.
                )",

                /* create */
                R"(
LyricExtender
                )",

                /* read */
                R"(
autoExtenders
extendersOverRests
                )",

                /* write */
                R"(

                )");
