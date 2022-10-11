/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2008--2022 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "axis-group-interface.hh"
#include "directional-element-interface.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"

#include "translator.icc"

#include <set>

using std::set;
using std::vector;

class Dynamic_align_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Dynamic_align_engraver);
  void acknowledge_rhythmic_head (Grob_info);
  void acknowledge_stem (Grob_info);
  void acknowledge_dynamic (Grob_info);
  void acknowledge_footnote_spanner (Grob_info);
  void acknowledge_end_dynamic (Grob_info_t<Spanner>);

protected:
  void stop_translation_timestep ();

private:
  void create_line_spanner (Grob *cause);
  void set_spanner_bounds (Spanner *line, bool end);
  Spanner *line_;
  Spanner *ended_line_; // Spanner manually broken, don't use it for new grobs
  Spanner *current_dynamic_spanner_;
  vector<Spanner *> ended_;
  vector<Spanner *> started_;
  vector<Grob *> scripts_;
  vector<Grob *> support_;

  set<Spanner *> running_;
};

Dynamic_align_engraver::Dynamic_align_engraver (Context *c)
  : Engraver (c)
{
  line_ = 0;
  ended_line_ = 0;
  current_dynamic_spanner_ = 0;
}

void
Dynamic_align_engraver::create_line_spanner (Grob *cause)
{
  if (!line_)
    line_ = make_spanner ("DynamicLineSpanner", cause->self_scm ());
}

void
Dynamic_align_engraver::acknowledge_end_dynamic (Grob_info_t<Spanner> info)
{
  auto *const sp = info.grob ();
  ended_.push_back (sp);

  if (!line_)
    return;

  /* If the break flag is set, store the current spanner and let new dynamics
   * create a new spanner
   */
  bool spanner_broken = (current_dynamic_spanner_ == sp)
                        && from_scm<bool> (get_property (sp, "spanner-broken"));
  if (spanner_broken)
    {
      if (ended_line_)
        programming_error ("already have a force-ended DynamicLineSpanner.");
      ended_line_ = line_;
      line_ = 0;
      current_dynamic_spanner_ = 0;
    }
}

void
Dynamic_align_engraver::acknowledge_footnote_spanner (Grob_info info)
{
  Grob *parent = info.grob ()->get_y_parent ();
  if (line_ && parent
      && parent->internal_has_interface (ly_symbol2scm ("dynamic-interface")))
    Axis_group_interface::add_element (line_, info.grob ());
}

void
Dynamic_align_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  support_.push_back (info.grob ());
}

void
Dynamic_align_engraver::acknowledge_stem (Grob_info info)
{
  support_.push_back (info.grob ());
}

void
Dynamic_align_engraver::acknowledge_dynamic (Grob_info info)
{
  Stream_event *cause = info.event_cause ();
  // Check whether an existing line spanner has the same direction
  if (line_ && cause)
    {
      Direction line_dir = get_grob_direction (line_);
      Direction grob_dir
        = from_scm<Direction> (get_property (cause, "direction"));

      // If we have an explicit direction for the new dynamic grob
      // that differs from the current line spanner, break the spanner
      if (grob_dir && (line_dir != grob_dir))
        {
          if (!ended_line_)
            ended_line_ = line_;
          line_ = 0;
          current_dynamic_spanner_ = 0;
        }
    }

  create_line_spanner (info.grob ());
  if (Spanner *sp = dynamic_cast<Spanner *> (info.grob ()))
    {
      started_.push_back (sp);
      current_dynamic_spanner_ = sp;
    }
  else if (Item *item = dynamic_cast<Item *> (info.grob ()))
    scripts_.push_back (item);
  else
    info.grob ()->programming_error ("unknown dynamic grob");

  Axis_group_interface::add_element (line_, info.grob ());

  if (cause)
    {
      if (Direction d = from_scm<Direction> (get_property (cause, "direction")))
        set_grob_direction (line_, d);
    }
}

void
Dynamic_align_engraver::set_spanner_bounds (Spanner *line, bool end)
{
  if (!line)
    return;

  for (const auto d : {LEFT, RIGHT})
    {
      if ((d == LEFT && !line->get_bound (LEFT))
          || (end && d == RIGHT && !line->get_bound (RIGHT)))
        {
          vector<Spanner *> const &spanners = (d == LEFT) ? started_ : ended_;

          Grob *bound = 0;
          if (scripts_.size ())
            bound = scripts_[0];
          else if (spanners.size ())
            bound = spanners[0]->get_bound (d);
          else
            {
              bound
                = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
              programming_error (
                "started DynamicLineSpanner but have no left bound");
            }

          line->set_bound (d, bound);
        }
    }
}

void
Dynamic_align_engraver::stop_translation_timestep ()
{
  for (vsize i = 0; i < started_.size (); i++)
    running_.insert (started_[i]);
  for (vsize i = 0; i < ended_.size (); i++)
    {
      Spanner *sp = ended_[i];

      set<Spanner *>::iterator it = running_.find (sp);
      if (it != running_.end ())
        running_.erase (it);
      else
        started_[i]->programming_error ("lost track of this dynamic spanner");
    }

  bool end = line_ && running_.empty ();

  // Set the proper bounds for the current spanner and for a spanner that
  // is ended now
  set_spanner_bounds (ended_line_, true);
  set_spanner_bounds (line_, end);
  // If the flag is set to break the spanner after the current child, don't
  // add any more support points (needed e.g. for style=none, where the
  // invisible spanner should NOT be shifted since we don't have a line).
  bool spanner_broken = current_dynamic_spanner_
                        && from_scm<bool> (get_property (
                          current_dynamic_spanner_, "spanner-broken"));
  for (vsize i = 0; line_ && !spanner_broken && i < support_.size (); i++)
    Side_position_interface::add_support (line_, support_[i]);

  if (end)
    {
      line_ = 0;
    }

  ended_line_ = 0;
  ended_.clear ();
  started_.clear ();
  scripts_.clear ();
  support_.clear ();
}

void
Dynamic_align_engraver::boot ()
{
  ADD_ACKNOWLEDGER (dynamic);
  ADD_ACKNOWLEDGER (rhythmic_head);
  ADD_ACKNOWLEDGER (stem);
  ADD_ACKNOWLEDGER (footnote_spanner);
  ADD_END_ACKNOWLEDGER (dynamic);
}

ADD_TRANSLATOR (Dynamic_align_engraver,
                /* doc */
                R"(
Align hairpins and dynamic texts on a horizontal line.
                )",

                /* create */
                R"(
DynamicLineSpanner
                )",

                /* read */
                R"(
currentMusicalColumn
                )",

                /* write */
                R"(

                )");
