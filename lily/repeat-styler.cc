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

#include "repeat-styler.hh"

#include "context.hh"
#include "input.hh"
#include "lily-imports.hh"
#include "music.hh"
#include "music-iterator.hh"

#include <string>

void
Repeat_styler::report_end_event (SCM event_sym, long alt_num, long repeat_count,
                                 long return_count)
{
  SCM ev_scm = Lily::make_music (event_sym);
  auto *const ev = unsmob<Music> (ev_scm);

  if (auto *const origin = owner ()->get_music ()->origin ())
    ev->set_spot (*origin);

  if (alt_num > 0)
    set_property (ev, "alternative-number", to_scm (alt_num));

  if (repeat_count > 0)
    set_property (ev, "repeat-count", to_scm (repeat_count));

  if (return_count >= 0)
    set_property (ev, "return-count", to_scm (return_count));

  // Currently, repeat-body-start-moment helps detect conflicting jumps.  In
  // the future, it might be used to engrave nested segno repeats in
  // conjunction with a mark table maintained by Mark_tracking_translator.
  // In that future, we would probably also want to report the point of the
  // first coda mark as repeat-body-end-moment.
  set_property (ev, "repeat-body-start-moment",
                to_scm (spanned_time ().left ()));

  ev->send_to_context (owner ()->get_context ());
  scm_remember_upto_here_1 (ev_scm);
}

void
Repeat_styler::report_alternative_event (Music *element, Direction d,
                                         size_t volta_depth, SCM volta_nums)
{
  SCM ev_scm = Lily::make_music (ly_symbol2scm ("AlternativeEvent"));
  auto *const ev = unsmob<Music> (ev_scm);
  if (element)
    {
      if (auto *origin = element->origin ())
        ev->set_spot (*origin);
    }
  set_property (ev, "alternative-dir", to_scm (d));
  set_property (ev, "volta-depth", to_scm (volta_depth));
  set_property (ev, "volta-numbers", volta_nums);
  ev->send_to_context (owner ()->get_context ());
  scm_remember_upto_here_1 (ev_scm);
}

class Null_repeat_styler final : public Repeat_styler
{
public:
  explicit Null_repeat_styler (Music_iterator *owner)
    : Repeat_styler (owner)
  {
  }

  void derived_report_start () override {}
  bool derived_report_alternative_group_start (Direction /*start*/,
                                               Direction /*end*/,
                                               bool /*in_order*/) override
  {
    return false; // disable volta brackets
  }
  void derived_report_alternative_start (Music *, long /*alt_num*/,
                                         size_t /*volta_depth*/,
                                         SCM /*volta_nums*/) override
  {
  }
  void derived_report_return (long /*alt_num*/, long /*return_count*/) override
  {
  }
  void derived_report_alternative_group_end (Music *,
                                             size_t /*volta_depth*/) override
  {
  }
};

class Segno_repeat_styler final : public Repeat_styler
{
private:
  bool coda_marks_enabled_ = true;

  void report_mark (Music *music, long alt_num) const
  {
    SCM ev_name = (alt_num == 0) ? ly_symbol2scm ("SegnoMarkEvent")
                                 : ly_symbol2scm ("CodaMarkEvent");
    SCM ev_scm = Lily::make_music (ev_name);
    auto *const ev = unsmob<Music> (ev_scm);
    if (auto *const origin = music->origin ())
      ev->set_spot (*origin);
    ev->send_to_context (owner ()->get_context ());
    scm_remember_upto_here_1 (ev_scm);
  }

public:
  explicit Segno_repeat_styler (Music_iterator *owner)
    : Repeat_styler (owner)
  {
  }

  void derived_report_start () override
  {
    if (repeat_count () < 2)
      return;

    report_mark (owner ()->get_music (), 0);
  }

  bool derived_report_alternative_group_start (Direction start, Direction end,
                                               bool in_order) override
  {
    if (repeat_count () < 2)
      return false; // disable volta brackets

    // Coda marks are sufficiently informative when the alternatives appear at
    // the tail of the repeated section and are performed in order.  The repeat
    // body must also not be empty.  In other cases, we fall back on volta
    // brackets and simplify our D.S. instructions.
    if (alternative_depth () == 1)
      {
        const bool aligned_at_start = (start == START);
        const bool aligned_at_end = (end == STOP);
        coda_marks_enabled_ = !aligned_at_start && aligned_at_end && in_order;
      }

    // ... and nested alternatives always get volta brackets.
    return !(coda_marks_enabled_ && (alternative_depth () < 2));
  }

  void derived_report_alternative_start (Music *element, long alt_num,
                                         size_t volta_depth,
                                         SCM volta_nums) override
  {
    if (repeat_count () < 2)
      return;

    if (coda_marks_enabled_ && (alternative_depth () < 2))
      {
        // In general, there is no reason to mark an empty passage.
        // Importantly, this allows "al Coda" structures where the final
        // alternative has no music and a section label is defined at the same
        // moment.
        const bool empty
          = !element
            || (!element->get_length () && !element->start_mom ().grace_part_);
        if (!empty)
          report_mark (element, alt_num);
      }
    else
      {
        report_alternative_event (element, (alt_num == 1) ? START : CENTER,
                                  volta_depth, volta_nums);
      }
  }

  void derived_report_return (long alt_num, long return_count) override
  {
    auto reps = repeat_count ();
    if (reps < 2)
      return;

    if (coda_marks_enabled_ && (alternative_depth () < 2))
      ; // Allow a detailed D.S. al ... instruction.
    else
      {
        // We have fallen back on notating alternatives with volta brackets.
        // Keep redundant information out of our D.S. instructions.
        alt_num = -1;
        reps = -1;
        return_count = -1;
      }

    report_end_event (ly_symbol2scm ("DalSegnoEvent"), alt_num, reps,
                      return_count);
  }

  void derived_report_alternative_group_end (Music *element,
                                             size_t volta_depth) override
  {
    if (repeat_count () < 2)
      return;

    if (coda_marks_enabled_ && (alternative_depth () < 2))
      ; // though marks are enabled, we don't mark the end
    else
      report_alternative_event (element, STOP, volta_depth, SCM_EOL);
  }
};

class Volta_repeat_styler final : public Repeat_styler
{
public:
  explicit Volta_repeat_styler (Music_iterator *owner)
    : Repeat_styler (owner)
  {
  }

  void derived_report_start () override
  {
    SCM ev_scm = Lily::make_music (ly_symbol2scm ("VoltaRepeatStartEvent"));
    auto *const ev = unsmob<Music> (ev_scm);

    if (auto *const origin = owner ()->get_music ()->origin ())
      ev->set_spot (*origin);

    if (repeat_count () > 0)
      set_property (ev, "repeat-count", to_scm (repeat_count ()));

    ev->send_to_context (owner ()->get_context ());
    scm_remember_upto_here_1 (ev_scm);
  }

  bool derived_report_alternative_group_start (Direction /*start*/,
                                               Direction /*end*/,
                                               bool /*in_order*/) override
  {
    if (repeat_count () < 2)
      return false; // disable volta brackets

    return true;
  }

  void derived_report_alternative_start (Music *element, long alt_num,
                                         size_t volta_depth,
                                         SCM volta_nums) override
  {
    if (repeat_count () < 2)
      return;

    report_alternative_event (element, (alt_num == 1) ? START : CENTER,
                              volta_depth, volta_nums);
  }

  void derived_report_return (long alt_num, long return_count) override
  {
    const auto reps = (alt_num < 1) ? repeat_count () : 0;
    report_end_event (ly_symbol2scm ("VoltaRepeatEndEvent"), alt_num, reps,
                      return_count);
  }

  void derived_report_alternative_group_end (Music *element,
                                             size_t volta_depth) override
  {
    if (repeat_count () < 2)
      return;

    report_alternative_event (element, STOP, volta_depth, SCM_EOL);
  }
};

std::unique_ptr<Repeat_styler>
Repeat_styler::create_null (Music_iterator *owner)
{
  return std::make_unique<Null_repeat_styler> (owner);
}

std::unique_ptr<Repeat_styler>
Repeat_styler::create_segno (Music_iterator *owner)
{
  return std::make_unique<Segno_repeat_styler> (owner);
}

std::unique_ptr<Repeat_styler>
Repeat_styler::create_volta (Music_iterator *owner)
{
  return std::make_unique<Volta_repeat_styler> (owner);
}
