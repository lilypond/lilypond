/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "context.hh"
#include "global-context.hh"
#include "grob-array.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "ly-scm-list.hh"
#include "ly-smob-list.hh"
#include "note-column.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "staff-symbol.hh"
#include "stream-event.hh"
#include "text-interface.hh"
#include "volta-bracket.hh"
#include "warn.hh"

#include "translator.icc"

#include <string>
#include <vector>

// State pertaining to volta spans at a specific depth of nested folded
// repeats.
class Volta_layer
{
public:
  Stream_event *start_ev_ = nullptr;
  Stream_event *stop_prev_ev_ = nullptr;
  Stream_event *stop_curr_ev_ = nullptr; // to handle an empty bracket
  Moment start_mom_;
  Moment stop_mom_;
  Spanner *bracket_ = nullptr;
  Spanner *end_bracket_ = nullptr;
  Spanner *spanner_ = nullptr;
  SCM text_ = SCM_EOL;
  bool start_bracket_this_timestep_ = false;

public:
  void gc_mark () const { scm_gc_mark (text_); }
};

struct Preinit_Volta_engraver
{
  // Entry [n] pertains to volta spans in the nth-deep folded repeat.  [0] is
  // used if \volta appears at the top level, which is not expected, but is
  // easily written.
  std::vector<Volta_layer> layers_;
};

/*
  Create Volta spanners, by reading repeatCommands  property, usually
  set by Volta_repeat_iterator.
*/
class Volta_engraver : Preinit_Volta_engraver, public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Volta_engraver);

protected:
  void acknowledge_bar_line (Grob_info_t<Item>);
  void listen_dal_segno (Stream_event *);
  void listen_fine (Stream_event *);
  void listen_volta_span (Stream_event *);

  void derived_mark () const override;
  void finalize () override;
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();

  bool acknowledged_bar_line_ = false;
  bool should_close_end_ = false;
};

void
Volta_engraver::derived_mark () const
{
  for (const auto &layer : layers_)
    layer.gc_mark ();
}

void
Volta_engraver::finalize ()
{
  layers_.clear ();
}

Volta_engraver::Volta_engraver (Context *c)
  : Engraver (c)
{
  // We need at least one layer to support manual repeat commands.
  // Others may be created as needed.
  layers_.resize (1);
}

void
Volta_engraver::listen_dal_segno (Stream_event *)
{
  should_close_end_ = true;
}

void
Volta_engraver::listen_fine (Stream_event *)
{
  should_close_end_ = true;
}

void
Volta_engraver::listen_volta_span (Stream_event *ev)
{
  const auto layer_no = from_scm<size_t> (get_property (ev, "volta-depth"), 0);
  if (layer_no >= layers_.size ())
    layers_.resize (layer_no + 1);
  auto &layer = layers_[layer_no];

  // It is common to have the same repeat structure in multiple voices, so we
  // ignore simultaneous events; but it might not be a bad thing to add some
  // consistency checks here if they could catch some kinds of user error.

  auto dir = from_scm<Direction> (get_property (ev, "span-direction"));
  if (dir == START)
    {
      if (!layer.start_ev_)
        layer.start_ev_ = ev;
    }
  else if (dir == STOP)
    {
      if (!layer.start_ev_)
        {
          if (!layer.stop_prev_ev_)
            layer.stop_prev_ev_ = ev;
        }
      else
        {
          // When an alternative is empty, we can in one timestep receive a stop
          // event for the previous alternative and the current alternative.
          // (This code will not handle consecutive empty alternatives, but it
          // covers the most important case: an empty final alternative.)
          SCM volta_numbers_sym = ly_symbol2scm ("volta-numbers");
          SCM start_nums = get_property (layer.start_ev_, volta_numbers_sym);
          SCM these_nums = get_property (ev, volta_numbers_sym);
          if (!ly_is_equal (these_nums, start_nums))
            {
              if (!layer.stop_prev_ev_)
                layer.stop_prev_ev_ = ev;
            }
          else
            {
              if (!layer.stop_curr_ev_)
                layer.stop_curr_ev_ = ev;
            }
        }
    }
  else
    {
      ev->programming_error ("invalid direction of volta-span-event");
    }
}

void
Volta_engraver::process_music ()
{
  for (size_t layer_no = 0; layer_no < layers_.size (); ++layer_no)
    {
      auto &layer = layers_[layer_no];

      // Ignore events for trivial repeats (when configured)
      for (auto ppev :
           {&layer.start_ev_, &layer.stop_prev_ev_, &layer.stop_curr_ev_})
        {
          if (!*ppev)
            continue;

          const auto rep_count
            = from_scm (get_property (*ppev, "repeat-count"), 1L);
          if ((rep_count < 2)
              && !from_scm<bool> (
                get_property (this, "printTrivialVoltaRepeats")))
            {
              *ppev = nullptr;
            }
        }

      bool manual_start = false;
      bool manual_end = false;

      if (layer_no == 0) // manual repeat commands
        {
          SCM repeat_commands = get_property (this, "repeatCommands");
          for (SCM c : as_ly_scm_list (repeat_commands))
            {
              if (scm_is_pair (c)
                  && scm_is_eq (scm_car (c), ly_symbol2scm ("volta"))
                  && scm_is_pair (scm_cdr (c)))
                {
                  SCM label = scm_cadr (c);
                  if (scm_is_false (label))
                    manual_end = true;
                  else
                    {
                      manual_start = true;
                      layer.text_ = label;
                    }
                }
            }
        }

      bool end = manual_end || layer.stop_prev_ev_;
      if (!end && layer.bracket_)
        {
          if (layer.stop_mom_ < Moment::infinity ())
            {
              // VoltaBracket.musical-length was specified.  Check it and
              // disregard voltaSpannerDuration.
              end = (now_mom () >= layer.stop_mom_);
            }
          else
            {
              auto voltaSpannerDuration
                = from_scm (get_property (this, "voltaSpannerDuration"),
                            Moment::infinity ());
              end = (voltaSpannerDuration <= now_mom () - layer.start_mom_);
            }
        }

      layer.start_bracket_this_timestep_ = manual_start || layer.start_ev_;
      if (layer.start_bracket_this_timestep_ && layer.bracket_ && !end)
        {
          if (manual_start)
            {
              layer.bracket_->warning (_ ("already have a VoltaBracket;"
                                          "ending it prematurely"));
            }
          end = true;
        }

      if (end)
        {
          if (layer.bracket_)
            {
              layer.end_bracket_ = layer.bracket_;
              layer.bracket_ = nullptr;
            }
          else if (manual_end)
            {
              // FIXME: Be more verbose?
              warning (_ ("no VoltaBracket to end"));
            }
        }

      if (layer.start_bracket_this_timestep_)
        {
          layer.start_mom_ = now_mom ();
          layer.stop_mom_ = Moment::infinity ();
          layer.bracket_ = make_spanner (
            "VoltaBracket",
            layer.start_ev_ ? to_scm (layer.start_ev_) : SCM_EOL);

          if (!layer.spanner_)
            {
              layer.spanner_ = make_spanner (
                "VoltaBracketSpanner",
                layer.start_ev_ ? to_scm (layer.start_ev_) : SCM_EOL);

              // Set the vertical order of the layers by adjusting
              // outside-staff-priority.
              if (layer_no)
                {
                  SCM sym = ly_symbol2scm ("outside-staff-priority");
                  SCM pri = get_property (layer.spanner_, sym);
                  set_property (layer.spanner_, sym,
                                scm_difference (pri, to_scm (layer_no)));
                }
            }

          Axis_group_interface::add_element (layer.spanner_, layer.bracket_);
        }
    }
}

void
Volta_engraver::acknowledge_bar_line (Grob_info_t<Item> info)
{
  acknowledged_bar_line_ = true;

  auto *const item = info.grob ();
  for (auto &layer : layers_)
    {
      if (layer.bracket_)
        Volta_bracket_interface::add_bar (layer.bracket_, item, LEFT);
      if (layer.end_bracket_)
        Volta_bracket_interface::add_bar (layer.end_bracket_, item, RIGHT);

      if (layer.spanner_)
        Side_position_interface::add_support (layer.spanner_, item);
    }

  // Certain bar lines cause volta brackets to hook down at the end.
  // See the function allow-volta-hook in bar-line.scm.
  if (!should_close_end_)
    {
      SCM glyph = get_property (item, "glyph-left");
      should_close_end_
        = !from_scm<bool> (Lily::volta_bracket_calc_hook_visibility (glyph));
    }
}

void
Volta_engraver::start_translation_timestep ()
{
  acknowledged_bar_line_ = false;
  should_close_end_ = false;
}

void
Volta_engraver::stop_translation_timestep ()
{
  auto *const ci = unsmob<Item> (get_property (this, "currentCommandColumn"));

  for (auto &layer : layers_)
    {
      if (layer.start_bracket_this_timestep_)
        {
          layer.stop_mom_
            = layer.start_mom_
              + from_scm (get_property (layer.bracket_, "musical-length"),
                          Moment::infinity ());

          // Cancel the bracket if it will not end during a future timestep.
          if (layer.stop_mom_ <= layer.start_mom_)
            {
              // Should we warn if VoltaBracket.musical-length was negative?
              layer.bracket_->suicide ();
              layer.bracket_ = nullptr;
              layer.start_bracket_this_timestep_ = false;
            }
          else if (layer.stop_mom_ < Moment::infinity ())
            {
              // VoltaBracket.musical-length is valid; use it.
              find_global_context ()->add_moment_to_process (layer.stop_mom_);
            }
          else
            {
              // VoltaBracket.musical-length is unlimited, so the bracket
              // continues until the end of the alternative or until
              // voltaSpannerDuration applies.
              if (layer.stop_curr_ev_) // the current alternative is empty
                {
                  layer.bracket_->suicide ();
                  layer.bracket_ = nullptr;
                  layer.start_bracket_this_timestep_ = false;
                }
            }
        }

      if (layer.start_bracket_this_timestep_)
        {
          if (!scm_is_null (layer.text_)) // explicit label from repeatCommands
            set_property (layer.bracket_, "text", layer.text_);

          if (layer.start_ev_)
            {
              SCM nums = get_property (layer.start_ev_, "volta-numbers");
              set_property (layer.bracket_, "volta-numbers", nums);
            }

          layer.start_bracket_this_timestep_ = false;
        }

      if (layer.end_bracket_)
        {
          if (!acknowledged_bar_line_)
            add_bound_item (layer.end_bracket_, ci);

          if (layer.spanner_)
            {
              layer.spanner_->set_bound (RIGHT,
                                         layer.end_bracket_->get_bound (RIGHT));
            }

          if (!layer.bracket_)
            {
              SCM staves_found = get_property (this, "stavesFound");
              for (auto *g : as_ly_smob_list<Grob> (staves_found))
                Side_position_interface::add_support (layer.spanner_, g);

              layer.spanner_ = 0;
            }

          // TODO: Now that we attempt to handle nested repeats, consider
          // whether there is a case in which one layer should have an end hook
          // and the other should not, and how important it is to get it right.
          if (!should_close_end_)
            {
              SCM eh_sym = ly_symbol2scm ("edge-height");
              auto eh = from_scm (get_property (layer.end_bracket_, eh_sym),
                                  Drul_array<Real> (2.0, 2.0));
              if (eh.back () != 0.0)
                {
                  eh.back () = 0.0;
                  set_property (layer.end_bracket_, eh_sym, to_scm (eh));
                }
            }

          announce_end_grob (layer.end_bracket_, SCM_EOL);
          layer.end_bracket_ = 0;
        }

      if (layer.bracket_ && !layer.bracket_->get_bound (LEFT))
        layer.bracket_->set_bound (LEFT, ci);

      if (layer.spanner_ && layer.bracket_ && !layer.spanner_->get_bound (LEFT))
        layer.spanner_->set_bound (LEFT, layer.bracket_->get_bound (LEFT));

      layer.start_ev_ = nullptr;
      layer.stop_prev_ev_ = nullptr;
      layer.stop_curr_ev_ = nullptr;
      layer.text_ = SCM_EOL;
    }
}

/*
  TODO: should attach volta to paper-column if no bar is found.
*/
void
Volta_engraver::boot ()
{
  ADD_ACKNOWLEDGER (bar_line);
  ADD_LISTENER (dal_segno);
  ADD_LISTENER (fine);
  ADD_LISTENER (volta_span);
}

ADD_TRANSLATOR (Volta_engraver,
                /* doc */
                R"(
Make volta brackets.
                )",

                /* create */
                R"(
VoltaBracket
VoltaBracketSpanner
                )",

                /* read */
                R"(
currentCommandColumn
printTrivialVoltaRepeats
repeatCommands
stavesFound
voltaSpannerDuration
                )",

                /* write */
                "");
