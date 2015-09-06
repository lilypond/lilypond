/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2015 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "multi-measure-rest.hh"
#include "paper-column.hh"
#include "engraver-group.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "moment.hh"
#include "spanner.hh"

#include "translator.icc"

/**
   The name says it all: make multi measure rests
*/
class Multi_measure_rest_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Multi_measure_rest_engraver);

protected:
  void process_music ();
  void start_translation_timestep ();
  DECLARE_TRANSLATOR_LISTENER (multi_measure_rest);
  DECLARE_TRANSLATOR_LISTENER (multi_measure_text);

private:
  void add_bound_item_to_grobs (Item *);
  void clear_lapsed_events (const Moment &now);

private:
  Stream_event *rest_ev_;
  vector<Stream_event *> text_events_;
  int start_measure_;
  Moment stop_moment_;

  bool first_time_;
  // Ugh, this is a kludge - need this for multi-measure-rest-grace.ly
  Item *last_command_item_;
  Spanner *mmrest_;

  // text_[0] is a MultiMeasureRestNumber grob
  // the rest are optional MultiMeasureRestText grobs
  vector<Spanner *> text_;
};

Multi_measure_rest_engraver::Multi_measure_rest_engraver ()
{
  last_command_item_ = 0;

  /*
    For the start of a score.
  */
  first_time_ = true;
  start_measure_ = 0;
  mmrest_ = 0;
  rest_ev_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Multi_measure_rest_engraver, multi_measure_rest);
void
Multi_measure_rest_engraver::listen_multi_measure_rest (Stream_event *ev)
{
  /* FIXME: Should use ASSIGN_EVENT_ONCE. Can't do that yet because of
     the kill-mm-rests hack in part-combine-iterator. */
  rest_ev_ = ev;
  const Moment now (now_mom ());
  stop_moment_ = now + get_event_length (rest_ev_, now);
  /*
  if (ASSIGN_EVENT_ONCE (rest_ev_, ev))
    stop_moment_ = now_mom () + get_event_length (rest_ev_);
  */

  clear_lapsed_events (now);
}

IMPLEMENT_TRANSLATOR_LISTENER (Multi_measure_rest_engraver, multi_measure_text);
void
Multi_measure_rest_engraver::listen_multi_measure_text (Stream_event *ev)
{
  text_events_.push_back (ev);
}

void
Multi_measure_rest_engraver::add_bound_item_to_grobs (Item *item)
{
  add_bound_item (mmrest_, item);
  for (vsize i = 0; i < text_.size (); ++i)
    add_bound_item (text_[i], item);
}

void
Multi_measure_rest_engraver::clear_lapsed_events (const Moment &now)
{
  if (now.main_part_ >= stop_moment_.main_part_)
    {
      rest_ev_ = 0;
      text_events_.clear ();
    }
}

void
Multi_measure_rest_engraver::process_music ()
{
  const bool measure_end
  = scm_is_string (get_property ("whichBar"))
    && (robust_scm2moment (get_property ("measurePosition"),
                           Moment (0)).main_part_ == Rational (0));

  if (measure_end || first_time_)
    {
      last_command_item_ = unsmob<Item> (get_property ("currentCommandColumn"));
    }

  // Finalize the current grobs.
  if (measure_end && mmrest_)
    {
      // Set the measure count now that it is known.
      int cur = scm_to_int (get_property ("internalBarNumber"));
      int num = cur - start_measure_;
      SCM num_scm (scm_from_int (num));

      mmrest_->set_property ("measure-count", num_scm);

      assert (!text_.empty ());
      Grob *g = text_[0];
      assert (g);
      if (scm_is_null (g->get_property ("text")))
        {
          SCM thres = get_property ("restNumberThreshold");
          int t = 1;
          if (scm_is_number (thres))
            t = scm_to_int (thres);

          if (num <= t)
            g->suicide ();
          else
            {
              SCM text
              = scm_number_to_string (num_scm, scm_from_int (10));
              g->set_property ("text", text);
            }
        }

      if (last_command_item_)
        add_bound_item_to_grobs (last_command_item_);

      mmrest_ = 0;
      text_.clear ();
    }

  // Create new grobs if a rest event is (still) active.
  if (rest_ev_ && !mmrest_)
    {
      mmrest_ = make_spanner ("MultiMeasureRest", rest_ev_->self_scm ());

      Spanner *sp
      = make_spanner ("MultiMeasureRestNumber", rest_ev_->self_scm ());
      text_.push_back (sp);

      if (text_events_.size ())
        {
          for (vsize i = 0; i < text_events_.size (); i++)
            {
              Stream_event *e = text_events_[i];
              Spanner *sp
              = make_spanner ("MultiMeasureRestText", e->self_scm ());
              SCM t = e->get_property ("text");
              SCM dir = e->get_property ("direction");
              sp->set_property ("text", t);
              if (is_direction (dir))
                sp->set_property ("direction", dir);

              text_.push_back (sp);
            }

          /*
            Stack different scripts.
          */
          for (DOWN_and_UP (d))
            {
              Grob *last = 0;
              for (vsize i = 0; i < text_.size (); i++)
                {
                  if (scm_is_eq (scm_from_int (d),
                                 text_[i]->get_property ("direction")))
                    {
                      if (last)
                        Side_position_interface::add_support (text_[i], last);
                      last = text_[i];
                    }
                }
            }
        }

      for (vsize i = 0; i < text_.size (); i++)
        {
          Side_position_interface::add_support (text_[i], mmrest_);
          text_[i]->set_parent (mmrest_, Y_AXIS);
          text_[i]->set_parent (mmrest_, X_AXIS);
        }

      text_events_.clear ();

      start_measure_
      = scm_to_int (get_property ("internalBarNumber"));

      if (last_command_item_)
        {
          add_bound_item_to_grobs (last_command_item_);
          last_command_item_ = 0;
        }
    }

  first_time_ = false;
}

void
Multi_measure_rest_engraver::start_translation_timestep ()
{
  clear_lapsed_events (now_mom ());
}

ADD_TRANSLATOR (Multi_measure_rest_engraver,
                /* doc */
                "Engrave multi-measure rests that are produced with"
                " @samp{R}.  It reads @code{measurePosition} and"
                " @code{internalBarNumber} to determine what number to print"
                " over the @ref{MultiMeasureRest}.",

                /* create */
                "MultiMeasureRest "
                "MultiMeasureRestNumber "
                "MultiMeasureRestText ",

                /* read */
                "internalBarNumber "
                "restNumberThreshold "
                "currentCommandColumn "
                "measurePosition "
                "whichBar ",

                /* write */
                ""
               );
