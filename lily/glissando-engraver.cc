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

#include "international.hh"
#include "pointer-group-interface.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"
#include "item.hh"

#include "translator.icc"

using std::string;
using std::vector;

class Glissando_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Glissando_engraver);

protected:
  void listen_glissando (Stream_event *);
  void acknowledge_note_column (Grob_info_t<Item>);
  void finalize () override;

  void stop_translation_timestep ();
  void process_music ();

private:
  vector<Spanner *> lines_;
  vector<Spanner *> kill_me_;
  bool start_glissandi_;
  bool stop_glissandi_;

  Stream_event *event_;
  vector<vsize> note_column_1;
  vector<vsize> note_column_2;
};

Glissando_engraver::Glissando_engraver (Context *c)
  : Engraver (c)
{
  event_ = 0;
  start_glissandi_ = false;
  stop_glissandi_ = false;
}

void
Glissando_engraver::listen_glissando (Stream_event *ev)
{
  assign_event_once (event_, ev);
}

void
Glissando_engraver::process_music ()
{
  if (event_)
    start_glissandi_ = true;
}

void
Glissando_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  auto *const g = info.grob ();
  if (from_scm<bool> (get_property (g, "glissando-skip")))
    return;

  if (stop_glissandi_)
    {
      extract_grob_set (g, "note-heads", note_heads);
      int glissando_index = 0;
      for (vsize i = 0; i < note_column_1.size (); i++)
        {
          if (note_column_2[i] >= note_heads.size ())
            {
              kill_me_.push_back (lines_[i]);
              announce_end_grob (lines_[i], SCM_EOL);
            }
          else
            {
              lines_[i]->set_bound (RIGHT, note_heads[note_column_2[i]]);
              set_property (lines_[i], "glissando-index",
                            to_scm (glissando_index));
              glissando_index++;
              announce_end_grob (lines_[i],
                                 note_heads[note_column_2[i]]->self_scm ());
            }
        }
      lines_.clear ();
      note_column_1.clear ();
      note_column_2.clear ();
      stop_glissandi_ = false;
    }

  if (start_glissandi_)
    {
      extract_grob_set (g, "note-heads", note_heads);
      SCM map = get_property (this, "glissandoMap");
      if (scm_is_null (map))
        for (vsize i = 0; i < note_heads.size (); i++)
          {
            note_column_1.push_back (i);
            note_column_2.push_back (i);
          }
      else
        for (SCM m = map; scm_is_pair (m); m = scm_cdr (m))
          {
            SCM candidate = scm_car (m);
            if (!scm_is_pair (candidate))
              continue;
            int n1 = from_scm (scm_car (candidate), -1);
            int n2 = from_scm (scm_cdr (candidate), -1);
            if ((n1 < 0) || (n2 < 0) || (size_t (n1) >= note_heads.size ()))
              continue;
            note_column_1.push_back (vsize (n1));
            note_column_2.push_back (vsize (n2));
          }
      for (vsize i = 0; i < note_column_1.size (); i++)
        {
          lines_.push_back (make_spanner ("Glissando", event_->self_scm ()));
          lines_.back ()->set_bound (LEFT, note_heads[note_column_1[i]]);
        }
    }
}

void
Glissando_engraver::stop_translation_timestep ()
{
  if (start_glissandi_)
    {
      if (stop_glissandi_)
        programming_error ("overwriting glissando");
      stop_glissandi_ = true;
      start_glissandi_ = false;
    }
  event_ = 0;
}

void
Glissando_engraver::finalize ()
{
  if (!lines_.empty ())
    {
      string msg = _ ("unterminated glissando");

      if (event_)
        event_->warning (msg);
      else
        warning (msg);

      for (vsize i = 0; i < lines_.size (); i++)
        lines_[i]->suicide ();
    }

  for (vsize i = 0; i < kill_me_.size (); i++)
    kill_me_[i]->suicide ();
}

void
Glissando_engraver::boot ()
{
  ADD_LISTENER (glissando);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Glissando_engraver,
                /* doc */
                R"(
Engrave glissandi.
                )",

                /* create */
                R"(
Glissando
                )",

                /* read */
                R"(
glissandoMap
                )",

                /* write */
                R"(

                )");
