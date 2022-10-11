/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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
#include "script-interface.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "moment.hh"
#include "spanner.hh"

#include "translator.icc"

using std::vector;

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
  void listen_multi_measure_rest (Stream_event *);
  void listen_multi_measure_text (Stream_event *);
  void listen_multi_measure_articulation (Stream_event *);

private:
  void add_bound_item_to_grobs (Item *);
  void clear_lapsed_events (const Moment &now);
  bool grobs_initialized () const { return mmrest_; }
  void initialize_grobs ();
  void reset_grobs ();
  void set_measure_count (int n);

private:
  vector<Stream_event *> text_events_;
  vector<Stream_event *> articulation_events_;
  // text_[0] is a MultiMeasureRestNumber grob
  // the rest are optional MultiMeasureRestText and MultiMeasureRestScript
  // grobs
  vector<Spanner *> text_;
  Stream_event *rest_ev_;
  Spanner *mmrest_;
  Moment stop_moment_;
  int start_measure_;
  // Ugh, this is a kludge - need this for multi-measure-rest-grace.ly
  Item *last_command_item_;
  bool first_time_;
  int number_threshold_;
};

Multi_measure_rest_engraver::Multi_measure_rest_engraver (Context *c)
  : Engraver (c),
    rest_ev_ (0),
    mmrest_ (0),
    start_measure_ (0),
    last_command_item_ (0),
    first_time_ (true)
{
}

void
Multi_measure_rest_engraver::listen_multi_measure_rest (Stream_event *ev)
{
  /* FIXME: Should use assign_event_once. Can't do that yet because of
     the kill-mm-rests hack in part-combine-iterator. */
  rest_ev_ = ev;
  const auto now = now_mom ();
  stop_moment_ = now + get_event_length (rest_ev_, now);
  /*
  if (assign_event_once (rest_ev_, ev))
    stop_moment_ = now_mom () + get_event_length (rest_ev_);
  */

  clear_lapsed_events (now);
}

void
Multi_measure_rest_engraver::listen_multi_measure_text (Stream_event *ev)
{
  text_events_.push_back (ev);
}

void
Multi_measure_rest_engraver::listen_multi_measure_articulation (
  Stream_event *ev)
{
  articulation_events_.push_back (ev);
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
      articulation_events_.clear ();
    }
}

void
Multi_measure_rest_engraver::initialize_grobs ()
{
  mmrest_ = make_spanner ("MultiMeasureRest", rest_ev_->self_scm ());
  text_.push_back (
    make_spanner ("MultiMeasureRestNumber", mmrest_->self_scm ()));

  if (articulation_events_.size ())
    {
      for (vsize i = 0; i < articulation_events_.size (); i++)
        {
          Stream_event *e = articulation_events_[i];
          Spanner *sp = make_spanner ("MultiMeasureRestScript", e->self_scm ());
          make_script_from_event (sp, context (),
                                  get_property (e, "articulation-type"), i);
          SCM dir = get_property (e, "direction");
          if (is_scm<Direction> (dir))
            set_property (sp, "direction", dir);

          text_.push_back (sp);
        }
    }

  if (text_events_.size ())
    {
      for (vsize i = 0; i < text_events_.size (); i++)
        {
          Stream_event *e = text_events_[i];
          Spanner *sp = make_spanner ("MultiMeasureRestText", e->self_scm ());
          SCM t = get_property (e, "text");
          SCM dir = get_property (e, "direction");
          set_property (sp, "text", t);
          if (is_scm<Direction> (dir))
            set_property (sp, "direction", dir);

          text_.push_back (sp);
        }
    }

  /*
    Stack different scripts.
  */
  for (const auto d : {DOWN, UP})
    {
      SCM dir = to_scm (d);
      Grob *last = 0;
      for (vsize i = 0; i < text_.size (); i++)
        {
          if (ly_is_equal (dir, get_property (text_[i], "direction")))
            {
              if (last)
                Side_position_interface::add_support (text_[i], last);
              last = text_[i];
            }
        }
    }

  for (vsize i = 0; i < text_.size (); i++)
    {
      Side_position_interface::add_support (text_[i], mmrest_);
      text_[i]->set_y_parent (mmrest_);
      text_[i]->set_x_parent (mmrest_);
    }
}

void
Multi_measure_rest_engraver::reset_grobs ()
{
  text_.clear ();
  mmrest_ = 0;
}

void
Multi_measure_rest_engraver::set_measure_count (int n)
{
  SCM n_scm = to_scm (n);
  assert (mmrest_);
  set_property (mmrest_, "measure-count", n_scm);

  Grob *g = text_[0]; // the MultiMeasureRestNumber
  assert (g);
  if (scm_is_null (get_property (g, "text")))
    {
      if (n <= number_threshold_)
        g->suicide ();
      else
        {
          SCM text = scm_number_to_string (n_scm, to_scm (10));
          set_property (g, "text", text);
        }
    }
}

void
Multi_measure_rest_engraver::process_music ()
{
  if (from_scm<bool> (get_property (this, "measureStartNow")) || first_time_)
    {
      last_command_item_
        = unsmob<Item> (get_property (this, "currentCommandColumn"));

      // Finalize the current grobs.
      if (grobs_initialized ())
        {
          int curr_measure
            = from_scm<int> (get_property (this, "internalBarNumber"));
          set_measure_count (curr_measure - start_measure_);
          if (last_command_item_)
            add_bound_item_to_grobs (last_command_item_);

          announce_end_grob (mmrest_, SCM_EOL);
          reset_grobs ();
        }
    }

  // Create new grobs if a rest event is (still) active.
  if (!grobs_initialized () && rest_ev_)
    {
      initialize_grobs ();
      text_events_.clear ();
      articulation_events_.clear ();

      if (last_command_item_)
        {
          add_bound_item_to_grobs (last_command_item_);
          last_command_item_ = 0;
        }

      start_measure_ = from_scm<int> (get_property (this, "internalBarNumber"));
      number_threshold_
        = from_scm (get_property (this, "restNumberThreshold"), 1);
    }

  first_time_ = false;
}

void
Multi_measure_rest_engraver::start_translation_timestep ()
{
  clear_lapsed_events (now_mom ());
}

void
Multi_measure_rest_engraver::boot ()
{
  ADD_LISTENER (multi_measure_rest);
  ADD_LISTENER (multi_measure_text);
  ADD_LISTENER (multi_measure_articulation);
}

ADD_TRANSLATOR (Multi_measure_rest_engraver,
                /* doc */
                R"(
Engrave multi-measure rests that are produced with @samp{R}.  It reads
@code{measureStartNow} and @code{internalBarNumber} to determine what number to
print over the @ref{MultiMeasureRest}.
                )",

                /* create */
                R"(
MultiMeasureRest
MultiMeasureRestNumber
MultiMeasureRestText
MultiMeasureRestScript
                )",

                /* read */
                R"(
internalBarNumber
restNumberThreshold
currentCommandColumn
measureStartNow
                )",

                /* write */
                R"(

                )");
