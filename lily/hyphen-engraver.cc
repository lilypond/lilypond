/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Glen Prideaux <glenprideaux@iname.com>,
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

#include "engraver.hh"
#include "international.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Hyphen_engraver : public Engraver
{
  Stream_event *ev_ = nullptr;
  Stream_event *finished_ev_ = nullptr;

  Item *syllable_ = nullptr;
  Item *last_syllable_ = nullptr;

  // A LyricHyphen or VowelTransition or LyricSpace, reset at every time step.
  Spanner *hyphen_ = nullptr;
  // A previously created LyricHyphen or ... awaiting completion.  Forgotten
  // about as soon as it finds a right bound.
  Spanner *finished_hyphen_ = nullptr;

public:
  TRANSLATOR_DECLARATIONS (Hyphen_engraver);

protected:
  void acknowledge_lyric_syllable (Grob_info_t<Item>);
  void listen_hyphen (Stream_event *);
  void listen_vowel_transition (Stream_event *);

  void finalize () override;

  void stop_translation_timestep ();
  void process_music ();
};

Hyphen_engraver::Hyphen_engraver (Context *c)
  : Engraver (c)
{
}

/*
   In a given time step, we expect not to have both a hyphen and a
   vowel transition as they would obviously collide.  If we have
   neither of these two, create a LyricSpace to put a constraint
   on the minimum distance between lyric words through spacing rods.

   We do not expect a LyricText in every time step, however: think
   of _ skips in lyrics.  We support "some _ _ -- words" just as
   well as "some -- _ _ words" (but "some -- _ -- _ words", with a
   duplicate hyphen, prints a warning).
*/

void
Hyphen_engraver::listen_hyphen (Stream_event *ev)
{
  assign_event_once (ev_, ev);
}

void
Hyphen_engraver::listen_vowel_transition (Stream_event *ev)
{
  assign_event_once (ev_, ev);
}

void
Hyphen_engraver::process_music ()
{
  if (ev_)
    {
      if (ev_->in_event_class ("vowel-transition-event"))
        hyphen_ = make_spanner ("VowelTransition", ev_->self_scm ());
      else
        hyphen_ = make_spanner ("LyricHyphen", ev_->self_scm ());
    }
}

void
Hyphen_engraver::acknowledge_lyric_syllable (Grob_info_t<Item> info)
{
  syllable_ = info.grob ();

  if (!hyphen_)
    hyphen_ = make_spanner ("LyricSpace", syllable_->self_scm ());

  if (finished_hyphen_)
    {
      finished_hyphen_->set_bound (RIGHT, syllable_);
      announce_end_grob (finished_hyphen_, to_scm (syllable_));
      finished_hyphen_ = nullptr;
      finished_ev_ = nullptr;
    }
}

void
Hyphen_engraver::stop_translation_timestep ()
{
  if (syllable_)
    last_syllable_ = syllable_;

  if (hyphen_)
    {
      if (last_syllable_)
        hyphen_->set_bound (LEFT, last_syllable_);
      else
        {
          hyphen_->warning ("hyphen or vowel transition has no syllable to "
                            "attach to on its left; removing it");
          hyphen_->suicide ();
        }
    }

  if (finished_hyphen_ && hyphen_)
    {
      // When we reach this, a hyphen is pending completion, and another
      // hyphen was created in the time step, conflicting with it.  The
      // one pending completion may be an automatically created LyricSpace,
      // in which case it is just removed.  This happens with "some _ -- words".
      // Otherwise, there are extraneous hyphens in the input (e.g.,
      // "some \vowelTransition _ -- words") and we should warn.
      if (!finished_hyphen_->internal_has_interface (
            ly_symbol2scm ("lyric-space-interface")))
        finished_hyphen_->warning ("this hyphen or vowel transition was "
                                   "overridden by a later one");
      finished_hyphen_->suicide ();
    }

  if (hyphen_)
    {
      finished_hyphen_ = hyphen_;
      finished_ev_ = ev_;
    }

  hyphen_ = nullptr;
  ev_ = nullptr;
  syllable_ = nullptr;
}

void
completize_hyphen (Spanner *sp)
{
  if (!sp->get_bound (RIGHT))
    {
      extract_item_set (sp, "heads", heads);
      if (heads.size ())
        sp->set_bound (RIGHT, heads.back ());
    }
}

void
Hyphen_engraver::finalize ()
{
  if (hyphen_)
    {
      completize_hyphen (hyphen_);

      if (!hyphen_->get_bound (RIGHT))
        {
          hyphen_->warning (_ ("removing unterminated hyphen"));
          hyphen_->suicide ();
        }

      hyphen_ = nullptr;
    }

  if (finished_hyphen_)
    {
      completize_hyphen (finished_hyphen_);

      if (!finished_hyphen_->get_bound (RIGHT))
        {
          if (finished_ev_)
            finished_hyphen_->warning (_ ("unterminated hyphen; removing"));
          finished_hyphen_->suicide ();
        }
      finished_hyphen_ = nullptr;
    }
}

void
Hyphen_engraver::boot ()
{
  ADD_LISTENER (hyphen);
  ADD_LISTENER (vowel_transition);
  ADD_ACKNOWLEDGER (lyric_syllable);
}

ADD_TRANSLATOR (Hyphen_engraver,
                /* doc */
                R"(
Create lyric hyphens, vowel transitions and distance constraints between words.
                )",

                /* create */
                R"(
LyricHyphen
LyricSpace
VowelTransition
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
