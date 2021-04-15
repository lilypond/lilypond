/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2021 Glen Prideaux <glenprideaux@iname.com>,
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

  Spanner *hyphen_ = nullptr;
  Spanner *finished_hyphen_ = nullptr;

public:
  TRANSLATOR_DECLARATIONS (Hyphen_engraver);

protected:

  void acknowledge_lyric_syllable (Grob_info);
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

void
Hyphen_engraver::listen_hyphen (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (ev_, ev);
}

void
Hyphen_engraver::listen_vowel_transition (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (ev_, ev);
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
Hyphen_engraver::acknowledge_lyric_syllable (Grob_info i)
{
  Item *item = dynamic_cast<Item *> (i.grob ());

  if (!hyphen_)
    hyphen_ = make_spanner ("LyricSpace", item->self_scm ());

  if (hyphen_)
    hyphen_->set_bound (LEFT, item);

  if (finished_hyphen_)
    finished_hyphen_->set_bound (RIGHT, item);
}

void
Hyphen_engraver::stop_translation_timestep ()
{
  if (finished_hyphen_ && finished_hyphen_->get_bound (RIGHT))
    {
      finished_hyphen_ = nullptr;
      finished_ev_ = nullptr;
    }

  if (finished_hyphen_ && hyphen_)
    {
      programming_error ("hyphen not finished yet");
      finished_hyphen_ = nullptr;
      finished_ev_ = nullptr;
    }

  if (hyphen_)
    {
      finished_hyphen_ = hyphen_;
      finished_ev_ = ev_;
    }

  hyphen_ = nullptr;
  ev_ = nullptr;
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
  ADD_LISTENER (Hyphen_engraver, hyphen);
  ADD_LISTENER (Hyphen_engraver, vowel_transition);
  ADD_ACKNOWLEDGER (Hyphen_engraver, lyric_syllable);
}

ADD_TRANSLATOR (Hyphen_engraver,
                /* doc */
                "Create lyric hyphens, vowel transitions and distance "
                "constraints between words.",

                /* create */
                "LyricHyphen "
                "LyricSpace "
                "VowelTransition ",

                /* read */
                "",

                /* write */
                "");
