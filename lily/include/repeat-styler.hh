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

#ifndef REPEAT_STYLER_HH
#define REPEAT_STYLER_HH

#include "lily-guile.hh"
#include "moment.hh"

#include <memory>

class Music;
class Music_iterator;

// Repeat_styler announces the boundaries of repeated sections on behalf of the
// iterators of \repeat and \alternative music.
//
// Each instance of Volta_repeat_iterator creates an instance of Repeat_styler,
// and each instance of Alternative_sequence_iterator uses the styler of its
// enclosing repeat.
class Repeat_styler
{
public:
  // Create a no-op styler.  Owner must not be null.
  static std::unique_ptr<Repeat_styler> create_null (Music_iterator *owner);

  // Create a styler for \repeat volta.  Owner must not be null.
  static std::unique_ptr<Repeat_styler> create_volta (Music_iterator *owner);

  virtual ~Repeat_styler () = default;

  // Return the lifetime of the repeat in the timeline of the score, which was
  // passed into report_start ().
  const Interval_t<Moment> &spanned_time () const { return spanned_time_; }

  // Tell whether report_return () has been called at least once.
  bool reported_return () const { return reported_return_; }

  // Report that a repeat has started.  spanned_time is the lifetime of the
  // repeat in the timeline of the score.
  void report_start (const Interval_t<Moment> &spanned_time)
  {
    spanned_time_ = spanned_time;
    derived_report_start ();
  }

  // Report that an alternative has started.  alt_num is the index (1-...) of
  // the alternative within its group and volta_nums is a list of the volta
  // numbers in which the alternative is used.
  void report_alternative_start (Music *alt, long alt_num, SCM volta_nums)
  {
    derived_report_alternative_start (alt, alt_num, volta_nums);
  }

  // Report that it is time to return to the start of the repeated section.
  // alt_number is the index (1-...) of the alternative that is ending, or 0
  // for a simple repeat with no alternatives.
  void report_return (long alt_num)
  {
    reported_return_ = true;
    derived_report_return (alt_num);
  }

  // Report that the last alternative of a group has ended.
  void report_alternative_group_end (Music *alt)
  {
    derived_report_alternative_group_end (alt);
  }

protected:
  explicit Repeat_styler (Music_iterator *owner) : owner_ (owner) {}
  Music_iterator *owner () const { return owner_; }

  virtual void derived_report_start () = 0;
  virtual void derived_report_alternative_start (Music *alt,
                                                 long alt_num,
                                                 SCM volta_nums) = 0;
  virtual void derived_report_return (long alt_num) = 0;
  virtual void derived_report_alternative_group_end (Music *element) = 0;

private:
  Music_iterator *owner_ = nullptr;
  Interval_t<Moment> spanned_time_ {Moment::infinity (), Moment::infinity ()};
  bool reported_return_ = false;
};

#endif // REPEAT_STYLER_HH
