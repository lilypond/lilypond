/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "note-column.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "staff-symbol.hh"
#include "text-interface.hh"
#include "volta-bracket.hh"
#include "warn.hh"

#include "translator.icc"

#include <string>

/*
  Create Volta spanners, by reading repeatCommands  property, usually
  set by Volta_repeat_iterator.
*/
class Volta_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Volta_engraver);
protected:

  void acknowledge_bar_line (Grob_info);
  void listen_volta_span (Stream_event *);
  static std::string format_numbers (SCM volta_numbers);

  void derived_mark () const override;
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();

  Moment started_mom_;
  Spanner *volta_bracket_ = nullptr;
  Spanner *end_volta_bracket_ = nullptr;
  Spanner *volta_spanner_ = nullptr;
  // list of lists: one list per active volta span
  SCM volta_numbers_ = SCM_EOL;
  // sorted, deduped union of volta_numbers_
  SCM groomed_volta_numbers_ = SCM_EOL;
  bool got_new_nums_this_timestep_ = false;
};

void
Volta_engraver::derived_mark () const
{
  scm_gc_mark (volta_numbers_);
  scm_gc_mark (groomed_volta_numbers_);
}

Volta_engraver::Volta_engraver (Context *c)
  : Engraver (c)
{
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

  if (volta_numbers == SCM_EOL)
    return result;

  size_t range_start = 0;
  size_t prev_num = 0;

  auto handle_num = [&] (size_t num)
  {
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
Volta_engraver::listen_volta_span (Stream_event *ev)
{
  got_new_nums_this_timestep_ = true;

  SCM nums = get_property (ev, "volta-numbers");

  auto dir = from_scm<Direction> (get_property (ev, "span-direction"));
  if (dir == START)
    {
      volta_numbers_ = scm_cons (nums, volta_numbers_);
    }
  else if (dir == STOP)
    {
      // We want to remove exactly one element that matches nums.  For coding
      // simplicity, we remove all matches and then restore some if there were
      // actually more than one.
      auto removed_count
        = as_ly_scm_list (volta_numbers_).remove_if ([nums] (SCM entry)
      {
        return from_scm<bool> (scm_equal_p (entry, nums));
      });

      for (size_t i = 1; i < removed_count; ++i)
        {
          volta_numbers_ = scm_cons (nums, volta_numbers_);
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
  // Simultaneous \volta commands do not necessarily change the union of active
  // volta numbers when a particular \volta begins or ends.  Figure out whether
  // there is an actual change now.
  if (got_new_nums_this_timestep_)
    {
      SCM nums = SCM_EOL;
      for (SCM more : as_ly_scm_list (volta_numbers_))
        {
          nums = Srfi_1::lset_union (Guile_user::equal, nums, more);
        }

      nums = scm_sort_list (nums, Guile_user::less);
      nums = Srfi_1::delete_duplicates (nums, Guile_user::equal);

      got_new_nums_this_timestep_
        = !from_scm<bool> (scm_equal_p (nums, groomed_volta_numbers_));

      if (got_new_nums_this_timestep_)
        {
          groomed_volta_numbers_ = nums;
        }
    }

  SCM cs = get_property (this, "repeatCommands");

  bool end = false;
  SCM bracket_text = SCM_EOL;

  while (scm_is_pair (cs))
    {
      SCM c = scm_car (cs);

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

      cs = scm_cdr (cs);
    }

  if (bracket_text == SCM_EOL) // no user-supplied label
    {
      // use an automatic label?
      if (got_new_nums_this_timestep_
          || (end && (groomed_volta_numbers_ != SCM_EOL)))
        {
          if (volta_bracket_)
            {
              end = true;
            }

          const auto &s = format_numbers (groomed_volta_numbers_);
          if (!s.empty ())
            {
              bracket_text = ly_string2scm (s);
            }
        }
    }

  if (volta_bracket_ && !end)
    {
      SCM l (get_property (this, "voltaSpannerDuration"));
      Moment now = now_mom ();

      bool early_stop = unsmob<Moment> (l)
                        && *unsmob<Moment> (l) <= now - started_mom_;

      end = end || early_stop;
    }

  if (end && !volta_bracket_)
    /* fixme: be more verbose.  */
    warning (_ ("cannot end volta spanner"));
  else if (end)
    {
      end_volta_bracket_ = volta_bracket_;
      volta_bracket_ = 0;
    }

  if (volta_bracket_
      && (scm_is_string (bracket_text) || scm_is_pair (bracket_text)))
    {
      warning (_ ("already have a volta spanner, ending that one prematurely"));

      if (end_volta_bracket_)
        {
          warning (_ ("also already have an ended spanner"));
          warning (_ ("giving up"));
          return;
        }

      end_volta_bracket_ = volta_bracket_;
      volta_bracket_ = 0;
    }

  if (!volta_bracket_
      && Text_interface::is_markup (bracket_text))
    {
      started_mom_ = now_mom ();

      volta_bracket_ = make_spanner ("VoltaBracket", SCM_EOL);

      // Allow \override Score.VoltaBracket.text = "foo".
      if (get_property (volta_bracket_, "text") == SCM_EOL)
        {
          set_property (volta_bracket_, "text", bracket_text);
        }

      if (!volta_spanner_)
        volta_spanner_ = make_spanner ("VoltaBracketSpanner", SCM_EOL);

      Axis_group_interface::add_element (volta_spanner_, volta_bracket_);
    }
}

void
Volta_engraver::acknowledge_bar_line (Grob_info i)
{
  Item *item = dynamic_cast<Item *> (i.grob ());
  if (volta_bracket_)
    Volta_bracket_interface::add_bar (volta_bracket_, item);
  if (end_volta_bracket_)
    Volta_bracket_interface::add_bar (end_volta_bracket_, item);

  if (volta_spanner_)
    Side_position_interface::add_support (volta_spanner_, i.grob ());
}

void
Volta_engraver::start_translation_timestep ()
{
  got_new_nums_this_timestep_ = false;
}

void
Volta_engraver::stop_translation_timestep ()
{
  auto *const ci = unsmob<Item> (get_property (this, "currentCommandColumn"));

  if (end_volta_bracket_ && !end_volta_bracket_->get_bound (RIGHT))
    end_volta_bracket_->set_bound (RIGHT, ci);

  if (volta_spanner_ && end_volta_bracket_)
    volta_spanner_->set_bound (RIGHT, end_volta_bracket_->get_bound (RIGHT));

  if (end_volta_bracket_ && !volta_bracket_)
    {
      for (SCM s = get_property (this, "stavesFound"); scm_is_pair (s); s = scm_cdr (s))
        Side_position_interface::add_support (volta_spanner_, unsmob<Grob> (scm_car (s)));

      volta_spanner_ = 0;
    }

  if (end_volta_bracket_)
    {
      announce_end_grob (end_volta_bracket_, SCM_EOL);
      end_volta_bracket_ = 0;
    }

  if (volta_bracket_ && !volta_bracket_->get_bound (LEFT))
    volta_bracket_->set_bound (LEFT, ci);

  if (volta_spanner_ && volta_bracket_ && !volta_spanner_->get_bound (LEFT))
    volta_spanner_->set_bound (LEFT, volta_bracket_->get_bound (LEFT));
}

/*
  TODO: should attach volta to paper-column if no bar is found.
*/
void
Volta_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Volta_engraver, bar_line);
  ADD_LISTENER (Volta_engraver, volta_span);
}

ADD_TRANSLATOR (Volta_engraver,
                /* doc */
                "Make volta brackets.",

                /* create */
                "VoltaBracket "
                "VoltaBracketSpanner ",

                /* read */
                "repeatCommands "
                "voltaSpannerDuration "
                "stavesFound ",

                /* write */
                ""
               );
