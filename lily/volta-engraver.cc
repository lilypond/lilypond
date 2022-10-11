/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "grob-array.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "ly-scm-list.hh"
#include "ly-smob-list.hh"
#include "note-column.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "staff-symbol.hh"
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
  Moment started_mom_;
  Spanner *bracket_ = nullptr;
  Spanner *end_bracket_ = nullptr;
  Spanner *spanner_ = nullptr;
  // list of lists: one list per active volta span
  SCM numbers_ = SCM_EOL;
  // sorted, deduped union of numbers_
  SCM groomed_numbers_ = SCM_EOL;
  bool got_new_nums_this_timestep_ = false;

public:
  void gc_mark () const
  {
    scm_gc_mark (numbers_);
    scm_gc_mark (groomed_numbers_);
  }
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
  static std::string format_numbers (SCM volta_numbers);

  void derived_mark () const override;
  void finalize () override;
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();

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

// TODO: The volta number formatter could be a Scheme procedure configured as a
// context property.  It might make life easier if the formatter received a
// list with ranges represented by pairs rather than making the formatter
// implement the range-discovery algorithm that is implemented here.
std::string
Volta_engraver::format_numbers (SCM volta_numbers)
{
  // Use a dash for runs of 3 or more.  Behind Bars has an example using "1.2."
  // (p.236) but otherwise doesn't say much about this.
  //
  // TODO: It seems that "1.2.3." might also be more readable than "1.-3."
  // Should there be a context property controlling how large a range should be
  // before using a dash?

  constexpr auto EN_DASH = "\u2013";
  constexpr auto HAIR_SPACE = "\u200a";

  std::string result;

  if (scm_is_null (volta_numbers))
    return result;

  size_t range_start = 0;
  size_t prev_num = 0;

  auto handle_num = [&] (size_t num) {
    if (range_start)
      {
        if (num != prev_num + 1) // end range
          {
            if (!result.empty ())
              result += HAIR_SPACE;
            result += std::to_string (range_start) + '.';
            result += (prev_num - range_start > 1) ? EN_DASH : HAIR_SPACE;
            result += std::to_string (prev_num) + '.';
            range_start = 0;
          }
      }
    else if (prev_num)
      {
        if (num != prev_num + 1)
          {
            if (!result.empty ())
              result += HAIR_SPACE;
            result += std::to_string (prev_num) + '.';
          }
        else
          {
            range_start = prev_num;
          }
      }

    prev_num = num;
  };

  for (auto num : as_ly_scm_list_t<size_t> (volta_numbers))
    {
      handle_num (num);
    }
  handle_num (0);

  return result;
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

  layer.got_new_nums_this_timestep_ = true;

  SCM nums = get_property (ev, "volta-numbers");

  auto dir = from_scm<Direction> (get_property (ev, "span-direction"));
  if (dir == START)
    {
      layer.numbers_ = scm_cons (nums, layer.numbers_);
    }
  else if (dir == STOP)
    {
      // We want to remove exactly one element that matches nums.  For coding
      // simplicity, we remove all matches and then restore some if there were
      // actually more than one.
      auto removed_count
        = as_ly_scm_list (layer.numbers_).remove_if ([nums] (SCM entry) {
            return from_scm<bool> (scm_equal_p (entry, nums));
          });

      for (size_t i = 1; i < removed_count; ++i)
        {
          layer.numbers_ = scm_cons (nums, layer.numbers_);
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

      // Simultaneous \volta commands do not necessarily change the union of
      // active volta numbers when a particular \volta begins or ends.  Figure
      // out whether there is an actual change now.
      //
      // TODO: Some of the complexity of this engraver was introduced in a
      // misstep with the initial \volta implementation.  It can likely be
      // simplified now that volta brackets are associated with \alternative
      // groups rather than every \volta spec.
      if (layer.got_new_nums_this_timestep_)
        {
          SCM nums = SCM_EOL;
          for (SCM more : as_ly_scm_list (layer.numbers_))
            {
              nums = Srfi_1::lset_union (Guile_user::equal, nums, more);
            }

          nums = scm_sort_list (nums, Guile_user::less);
          nums = Srfi_1::delete_duplicates (nums, Guile_user::equal);

          layer.got_new_nums_this_timestep_
            = !from_scm<bool> (scm_equal_p (nums, layer.groomed_numbers_));

          if (layer.got_new_nums_this_timestep_)
            {
              layer.groomed_numbers_ = nums;
            }
        }

      bool end = false;
      SCM bracket_text = SCM_EOL;

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
                    end = true;
                  else
                    bracket_text = label;
                }
            }
        }

      if (scm_is_null (bracket_text)) // no user-supplied label
        {
          // use an automatic label?
          if (layer.got_new_nums_this_timestep_
              || (end && !scm_is_null (layer.groomed_numbers_)))
            {
              if (layer.bracket_)
                {
                  end = true;
                }

              const auto &s = format_numbers (layer.groomed_numbers_);
              if (!s.empty ())
                {
                  bracket_text = ly_string2scm (s);
                }
            }
        }

      if (layer.bracket_ && !end)
        {
          auto voltaSpannerDuration = from_scm (
            get_property (this, "voltaSpannerDuration"), Moment::infinity ());
          end = (voltaSpannerDuration <= now_mom () - layer.started_mom_);
        }

      if (end && !layer.bracket_)
        /* fixme: be more verbose.  */
        warning (_ ("cannot end volta spanner"));
      else if (end)
        {
          layer.end_bracket_ = layer.bracket_;
          layer.bracket_ = 0;
        }

      if (layer.bracket_
          && (scm_is_string (bracket_text) || scm_is_pair (bracket_text)))
        {
          warning (_ ("already have a volta spanner, "
                      "ending that one prematurely"));

          if (layer.end_bracket_)
            {
              warning (_ ("also already have an ended spanner"));
              warning (_ ("giving up"));
              return;
            }

          layer.end_bracket_ = layer.bracket_;
          layer.bracket_ = 0;
        }

      if (!layer.bracket_ && Text_interface::is_markup (bracket_text))
        {
          layer.started_mom_ = now_mom ();

          layer.bracket_ = make_spanner ("VoltaBracket", SCM_EOL);

          // Allow \override Score.VoltaBracket.text = "foo".
          if (scm_is_null (get_property (layer.bracket_, "text")))
            {
              set_property (layer.bracket_, "text", bracket_text);
            }

          if (!layer.spanner_)
            layer.spanner_ = make_spanner ("VoltaBracketSpanner", SCM_EOL);

          Axis_group_interface::add_element (layer.spanner_, layer.bracket_);
        }
    }
}

void
Volta_engraver::acknowledge_bar_line (Grob_info_t<Item> info)
{
  auto *const item = info.grob ();
  for (auto &layer : layers_)
    {
      if (layer.bracket_)
        Volta_bracket_interface::add_bar (layer.bracket_, item);
      if (layer.end_bracket_)
        Volta_bracket_interface::add_bar (layer.end_bracket_, item);

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
  for (auto &layer : layers_)
    layer.got_new_nums_this_timestep_ = false;

  should_close_end_ = false;
}

void
Volta_engraver::stop_translation_timestep ()
{
  auto *const ci = unsmob<Item> (get_property (this, "currentCommandColumn"));

  for (auto &layer : layers_)
    {
      if (layer.end_bracket_ && !layer.end_bracket_->get_bound (RIGHT))
        layer.end_bracket_->set_bound (RIGHT, ci);

      if (layer.spanner_ && layer.end_bracket_)
        layer.spanner_->set_bound (RIGHT,
                                   layer.end_bracket_->get_bound (RIGHT));

      if (layer.end_bracket_ && !layer.bracket_)
        {
          SCM staves_found = get_property (this, "stavesFound");
          for (auto *g : as_ly_smob_list<Grob> (staves_found))
            Side_position_interface::add_support (layer.spanner_, g);

          layer.spanner_ = 0;
        }

      if (layer.end_bracket_)
        {
          // TODO: Now that we attempt to handle nested repeats, consider
          // whether there is a case in which one layer should have an end hook
          // and the other should not, and how important it is to get it right.
          if (!should_close_end_)
            {
              SCM eh = get_property (layer.end_bracket_, "edge-height");
              eh = scm_cons (scm_car (eh), to_scm (0));
              set_property (layer.end_bracket_, "edge-height", eh);
            }

          announce_end_grob (layer.end_bracket_, SCM_EOL);
          layer.end_bracket_ = 0;
        }

      if (layer.bracket_ && !layer.bracket_->get_bound (LEFT))
        layer.bracket_->set_bound (LEFT, ci);

      if (layer.spanner_ && layer.bracket_ && !layer.spanner_->get_bound (LEFT))
        layer.spanner_->set_bound (LEFT, layer.bracket_->get_bound (LEFT));
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
repeatCommands
stavesFound
voltaSpannerDuration
                )",

                /* write */
                "");
