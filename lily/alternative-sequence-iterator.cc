/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "alternative-sequence-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "ly-scm-list.hh"
#include "ly-smob-list.hh"
#include "music.hh"
#include "repeat-styler.hh"
#include "volta-repeat-iterator.hh"

#include <memory>

void
Alternative_sequence_iterator::derived_mark () const
{
  scm_gc_mark (alt_restores_);
  Sequential_iterator::derived_mark ();
}

void
Alternative_sequence_iterator::create_children ()
{
  Sequential_iterator::create_children ();

  // Note: \alternative can be used with \repeat unfold in ly code, but those
  // are transformed before the music is iterated; therefore, searching here
  // for the nearest enclosing folded repeat is the same as searching for the
  // nearest enclosing repeat.
  auto *const repeat_iter = dynamic_cast<Volta_repeat_iterator *> (
    find_above_by_music_type (ly_symbol2scm ("folded-repeated-music")));

  repeat_styler_ = repeat_iter ? repeat_iter->get_repeat_styler ()
                               : Repeat_styler::create_null (this); // defensive
}

// Peek at the alternatives to figure out how they should be presented.
void
Alternative_sequence_iterator::analyze ()
{
  Direction start_alignment = CENTER;
  Direction end_alignment = CENTER;

  {
    // This won't compute the correct endpoint inside \grace.
    const auto start = get_context ()->now_mom ();
    const auto len = music_get_length () - music_start_mom ();
    const auto end = start + len;

    // Do these alternatives start at the start of the repeated section?
    if (start == repeat_styler_->spanned_time ().left ())
      start_alignment = START;

    // Do these alternatives end at the end of the repeated section?
    if (end == repeat_styler_->spanned_time ().right ())
      end_alignment = STOP;
  }

  bool alts_in_order = true;

  {
    const auto repeat_count
      = from_scm<size_t> (get_property (this, "repeat-count"), 1);
    SCM alts = get_property (get_music (), "elements");
    size_t next_expected_volta_num = 1;
    for (auto *alt : as_ly_smob_list<Music> (alts))
      {
        alt_info_.emplace_back ();
        auto &info = alt_info_.back ();

        SCM volta_nums = alt ? get_property (alt, "volta-numbers") : SCM_EOL;
        if (!scm_is_pair (volta_nums))
          {
            alt->warning (_ ("missing volta specification on alternative"
                             " element"));
            alts_in_order = false;
          }
        else
          {
            volta_nums = scm_sort_list (volta_nums, Guile_user::less);
            for (auto num : as_ly_scm_list_t<size_t> (volta_nums))
              {
                // In tail alternatives, we repeat after every volta except the
                // last.
                if ((end_alignment == STOP) && (num < repeat_count))
                  ++info.return_count;

                if (num == next_expected_volta_num)
                  ++next_expected_volta_num;
                else
                  alts_in_order = false;
              }
          }
      }

    if (alts_in_order && (next_expected_volta_num == repeat_count + 1)
        && (alt_info_.size () == 1))
      {
        // The same alternative is used for all volte.  A coda mark would
        // mislead: no material needs to be skipped.  We don't want to
        // complicate the segno styler to handle this exception: a user who
        // wants something like "D.C. 2 V." without a coda mark can use a
        // simple \repeat without \alternative.  We fall back to a bracket.
        alts_in_order = false;
      }
  }

  volta_brackets_enabled_ = repeat_styler_->report_alternative_group_start (
    start_alignment, end_alignment, alts_in_order);

  // The local volta bracket depth is whatever it was for the nearest enclosing
  // \alternative, plus one if volta brackets are enabled here.
  volta_bracket_depth_ = volta_brackets_enabled_ ? 1 : 0;
  for (auto *mi = get_parent (); mi; mi = mi->get_parent ())
    {
      if (auto *asi = dynamic_cast<Alternative_sequence_iterator *> (mi))
        {
          volta_bracket_depth_ += asi->volta_bracket_depth ();
          break;
        }
    }
}

void
Alternative_sequence_iterator::end_alternative ()
{
  if (done_count_ > alt_info_.size ()) // paranoia
    return;

  const auto &info = alt_info_[done_count_ - 1];
  if (info.return_count > 0)
    repeat_styler_->report_return (done_count_, info.return_count);

  if (done_count_ == alt_info_.size ()) // ending the final alternative
    {
      repeat_styler_->report_alternative_group_end (get_music (),
                                                    volta_bracket_depth_);
    }
  else if (done_count_ < alt_info_.size ()) // ending an earlier alternative
    {
      if (from_scm<bool> (get_property (get_context (), "timing")))
        restore_context_properties ();
    }
}

void
Alternative_sequence_iterator::restore_context_properties ()
{
  for (SCM ls : as_ly_scm_list (alt_restores_))
    {
      // Repeats may have different grace timing, so we need to adjust the
      // measurePosition grace timing to that of the current alternative rather
      // than that of the first.  The Timing_translator does this already but
      // is too late to avoid bad side-effects.
      //
      // TODO: This special case is ugly on its own, but more so because
      // Timing_translator saves and restores currentBarNumber depending on
      // alternativeNumberingStyle.  Why not either move that here or let
      // Timing_translator handle all timing-related properties?
      SCM mp_sym = ly_symbol2scm ("measurePosition");
      if (scm_is_eq (scm_cadr (ls), mp_sym))
        {
          Moment mp (unsmob<Moment> (scm_caddr (ls))->main_part_,
                     get_context ()->now_mom ().grace_part_);
          Lily::ly_context_set_property_x (scm_car (ls), mp_sym,
                                           mp.smobbed_copy ());
        }
      else
        {
          scm_apply_0 (Lily::ly_context_set_property_x, ls);
        }
    }
}

void
Alternative_sequence_iterator::save_context_properties ()
{
  // Save the starting values of specified context properties.  These will
  // be restored at the end of each alternative but the last.
  //
  // TODO: Some possibilities seem unaccounted for:
  //
  //   * A property may be defined at multiple levels of the context tree, but
  //     this records only the innermost.
  //
  //   * If one of these properties is undefined now, but something within
  //     an alternative element defines it, it will remain defined.
  //
  SCM prop_syms = get_property (get_context (), "alternativeRestores");
  for (SCM sym : as_ly_scm_list (prop_syms))
    {
      SCM val = SCM_EOL;
      if (auto *const c = where_defined (get_context (), sym, &val))
        {
          alt_restores_
            = scm_cons (ly_list (c->self_scm (), sym, val), alt_restores_);
        }
    }
}

void
Alternative_sequence_iterator::start_alternative ()
{
  if (done_count_ >= alt_info_.size ())
    return;

  // Examining the child music is ugly but effective.
  auto *const child = get_child ();
  auto *const music = child ? child->get_music () : nullptr;

  SCM volta_nums = music ? get_property (music, "volta-numbers") : SCM_EOL;
  if (!scm_is_pair (volta_nums))
    {
      // We already warned about this in analyze().
      volta_nums = SCM_EOL;
    }

  repeat_styler_->report_alternative_start (music, (done_count_ + 1),
                                            volta_bracket_depth_, volta_nums);
}

void
Alternative_sequence_iterator::process (Moment m)
{
  if (first_time_)
    {
      first_time_ = false;
      analyze ();

      if (alt_info_.size () > 1)
        {
          // TODO: Ignoring context properties when timing is disabled is the
          // legacy behavior, but it is questionable.  Wouldn't we want to
          // restore lastChord even in cadenza mode?  Why shouldn't timing be
          // saved and restored like other properties?  Do we need to handle
          // timing-related properties separately from lastChord etc.?
          if (from_scm<bool> (get_property (get_context (), "timing")))
            save_context_properties ();
        }

      start_alternative ();
    }

  Sequential_iterator::process (m);
}

void
Alternative_sequence_iterator::next_element ()
{
  done_count_++;
  Sequential_iterator::next_element ();
  end_alternative ();
  start_alternative ();
}

IMPLEMENT_CTOR_CALLBACK (Alternative_sequence_iterator);
