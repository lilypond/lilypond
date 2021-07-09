/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "sequential-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "ly-scm-list.hh"
#include "music.hh"

// iterator for \alternative {...}
class Alternative_sequence_iterator final : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Alternative_sequence_iterator () = default;

protected:
  void next_element () override;
  void create_children () override;
  void process (Moment) override;
  void derived_mark () const override;

  void add_repeat_command (SCM);
  void end_alternative ();
  void report_alternative_event (Music *element, Direction, SCM volta_nums);
  void restore_context_properties ();
  void save_context_properties ();
  void start_alternative ();

private:
  bool alts_need_end_repeat_ = false;
  bool final_alt_needs_end_repeat_ = false;
  bool first_time_ = true;
  long alt_count_ = 0;
  long done_count_ = 0;
  SCM alt_restores_ = SCM_EOL;
};

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

  alt_count_ = scm_ilength (get_property (get_music (), "elements"));
  done_count_ = 0;

  // TODO: This property is ugly.  Instead, check whether this iterator's end
  // time coincides with the repeat iterator's end time.  This is one thing
  // (not necessarily the only thing) that blocks the use of \alternative {}
  // end-justified within the body of \repeat {}.
  if (from_scm (get_property (get_music (), "alternative-dir"), CENTER) == STOP)
    alts_need_end_repeat_ = true;
}

void
Alternative_sequence_iterator::add_repeat_command (SCM what)
{
  SCM reps = ly_symbol2scm ("repeatCommands");
  SCM current_reps = SCM_EOL;
  auto *const where = where_defined (get_context (), reps, &current_reps);

  if (where && ly_cheap_is_list (current_reps))
    {
      current_reps = scm_cons (what, current_reps);
      set_property (where, reps, current_reps);
    }
}

void
Alternative_sequence_iterator::end_alternative ()
{
  if (done_count_ == alt_count_) // ending the final alternative
    {
      if (final_alt_needs_end_repeat_)
        add_repeat_command (ly_symbol2scm ("end-repeat"));

      report_alternative_event (get_music (), STOP, SCM_EOL);
    }
  else if (done_count_ < alt_count_) // ending an earlier alternative
    {
      if (alts_need_end_repeat_)
        add_repeat_command (ly_symbol2scm ("end-repeat"));

      if (from_scm<bool> (get_property (get_context (), "timing")))
        restore_context_properties ();
    }
}

void
Alternative_sequence_iterator::report_alternative_event (Music *element,
                                                         Direction d,
                                                         SCM volta_nums)
{
  auto *const ev = make_music_by_name (ly_symbol2scm ("AlternativeEvent"));
  if (element)
    {
      if (auto *origin = element->origin ())
        ev->set_spot (*origin);
    }
  set_property (ev, "alternative-dir", to_scm (d));
  set_property (ev, "alternative-increment", scm_length (volta_nums));
  report_event (ev);
  ev->unprotect ();
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
          Lily::ly_context_set_property_x (scm_car (ls),
                                           mp_sym, mp.smobbed_copy ());
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
            = scm_cons (scm_list_3 (c->self_scm (), sym, val),
                        alt_restores_);
        }
    }
}

void
Alternative_sequence_iterator::start_alternative ()
{
  if (done_count_ >= alt_count_)
    return;

  // Examining the child music is ugly but effective.
  auto *const child = get_child ();
  auto *const music = child ? child->get_music () : nullptr;

  SCM volta_nums = music ? get_property (music, "volta-numbers") : SCM_EOL;
  if (!scm_is_pair (volta_nums))
    {
      music->warning (_ ("missing volta specification on alternative element"));
      volta_nums = SCM_EOL;
    }

  report_alternative_event (music, (done_count_ ? CENTER : START), volta_nums);

  if ((done_count_ + 1) == alt_count_) // starting the final alternative
    {
      // If alternatives need end-repeat bars, even the final alternative needs
      // an end-repeat bar if it applies to any volta other than the final one.
      if (!alts_need_end_repeat_)
        {
          // No alternatives need an end repeat, including this one.
        }
      else if (!scm_is_pair (volta_nums))
        {
          // We already warned above.  Not finding a label, we'll continue as
          // if the final alternative is for the final volta only.
        }
      else if (scm_is_pair (scm_cdr (volta_nums)))
        {
          final_alt_needs_end_repeat_ = true;
        }
      else // a single, specified volta
        {
          SCM rep_count = get_property (this, "repeat-count");
          final_alt_needs_end_repeat_
            = scm_is_false (scm_equal_p (scm_car (volta_nums), rep_count));
        }
    }
}

void
Alternative_sequence_iterator::process (Moment m)
{
  if (first_time_)
    {
      first_time_ = false;

      if (alt_count_ > 1)
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
