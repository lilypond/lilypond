/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "pointer-group-interface.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "context.hh"
#include "spanner.hh"
#include "item.hh"

/**
   Create line-spanner grobs for lines that connect note heads.

   TODO: have the line commit suicide if the notes are connected with
   either slur or beam.
*/
class Note_head_line_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Note_head_line_engraver);

protected:
  void acknowledge_rhythmic_head (Grob_info);
  void process_acknowledged ();
  void stop_translation_timestep ();

private:
  Spanner *line_;
  Context *last_staff_;
  bool follow_;
  Grob *head_;
  Grob *last_head_;
};

Note_head_line_engraver::Note_head_line_engraver (Context *c)
  : Engraver (c)
{
  line_ = 0;
  follow_ = false;
  head_ = 0;
  last_head_ = 0;
  last_staff_ = 0;
}

void
Note_head_line_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  head_ = info.grob ();
  Context *tr = find_context_above (context (), ly_symbol2scm ("Staff"));
  if (tr && tr != last_staff_
      && from_scm<bool> (get_property (this, "followVoice")))
    {
      if (last_head_)
        follow_ = true;
    }
  last_staff_ = tr;
}

void
Note_head_line_engraver::process_acknowledged ()
{
  if (!line_ && follow_ && last_head_ && head_)
    {
      /* TODO: Don't follow if there's a beam.

      We can't do beam-stuff here, since beam doesn't exist yet.
      Should probably store follow_ in line_, and suicide at some
      later point */
      if (follow_)
        line_ = make_spanner ("VoiceFollower", head_->self_scm ());

      line_->set_bound (LEFT, last_head_);
      line_->set_bound (RIGHT, head_);

      follow_ = false;
    }
}

void
Note_head_line_engraver::stop_translation_timestep ()
{
  line_ = 0;
  if (head_)
    last_head_ = head_;
  head_ = 0;
}

#include "translator.icc"

void
Note_head_line_engraver::boot ()
{
  ADD_ACKNOWLEDGER (rhythmic_head);
}

ADD_TRANSLATOR (Note_head_line_engraver,
                /* doc */
                R"(
Engrave a line between two note heads in a staff switch if @code{followVoice}
is set.
                )",

                /* create */
                R"(
VoiceFollower
                )",

                /* read */
                R"(
followVoice
                )",

                /* write */
                R"(

                )");
