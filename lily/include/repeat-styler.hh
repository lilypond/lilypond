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

#ifndef REPEAT_STYLER_HH
#define REPEAT_STYLER_HH

#include "direction.hh"
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

  // Create a styler for \repeat segno.  Owner must not be null.
  static std::unique_ptr<Repeat_styler> create_segno (Music_iterator *owner);

  // Create a styler for \repeat volta.  Owner must not be null.
  static std::unique_ptr<Repeat_styler> create_volta (Music_iterator *owner);

  virtual ~Repeat_styler () = default;

  // Return the lifetime of the repeat in the timeline of the score, which was
  // passed into report_start ().
  const Interval_t<Moment> &spanned_time () const { return spanned_time_; }

  // Tell whether report_return () has been called at least once.
  bool reported_return () const { return reported_return_; }

  // Report that a repeat has started.  spanned_time is the lifetime of the
  // repeat in the timeline of the score.  repeat_count is the number of times
  // the section is performed from this starting point.
  void report_start (const Interval_t<Moment> &spanned_time, long repeat_count)
  {
    spanned_time_ = spanned_time;
    repeat_count_ = repeat_count;
    derived_report_start ();
  }

  // Report that an alternative group is starting.
  //
  // The start direction is START if the start of the alternative group is
  // aligned with the start of the repeated section, otherwise CENTER.
  //
  // The end direction is STOP if the end of the alternative group is aligned
  // with the end of the repeated section, otherwise CENTER.
  //
  // in_order is true if the alternatives are performed in order.
  //
  // This returns true if the styler has determined that volta brackets should
  // be enabled over this group of alternatives.
  bool report_alternative_group_start (Direction start, Direction end,
                                       bool in_order)
  {
    ++alternative_depth_;
    return derived_report_alternative_group_start (start, end, in_order);
  }

  // Report that an alternative has started.  alt_num is the index (1-...) of
  // the alternative within its group.  volta_depth is the depth of the
  // alternative in the repeat structure.  volta_nums is a list of the volta
  // numbers in which the alternative is used.
  void report_alternative_start (Music *alt, long alt_num, size_t volta_depth,
                                 SCM volta_nums)
  {
    derived_report_alternative_start (alt, alt_num, volta_depth, volta_nums);
  }

  // Report that it is time to return to the start of the repeated section.
  // alt_number is the index (1-...) of the alternative that is ending, or 0
  // for a simple repeat with no alternatives.  return_count is the number of
  // times this return is performed.
  void report_return (long alt_num, long return_count)
  {
    // When two \alternative groups are nested and both are end-aligned, we
    // report returns for the deeper one and then remain silent when the outer
    // one tries to report.
    if (alternative_depth_ < reported_return_depth_)
      return;
    reported_return_depth_ = alternative_depth_;
    reported_return_ = true;
    derived_report_return (alt_num, return_count);
  }

  // Report that the last alternative of a group has ended.
  void report_alternative_group_end (Music *alt, size_t volta_depth)
  {
    derived_report_alternative_group_end (alt, volta_depth);

    if (alternative_depth_ > 0) // paranoia
      {
        --alternative_depth_;
        if (!alternative_depth_)
          reported_return_depth_ = 0;
      }
  }

protected:
  explicit Repeat_styler (Music_iterator *owner)
    : owner_ (owner)
  {
  }
  Music_iterator *owner () const { return owner_; }

  long repeat_count () const { return repeat_count_; }

  size_t alternative_depth () const { return alternative_depth_; }

  void report_alternative_event (Music *element, Direction d,
                                 size_t volta_depth, SCM volta_nums);
  void report_end_event (SCM event_sym, long alt_num, long repeat_count,
                         long return_count);

  virtual void derived_report_start () = 0;
  virtual bool derived_report_alternative_group_start (Direction start,
                                                       Direction end,
                                                       bool in_order)
    = 0;
  virtual void derived_report_alternative_start (Music *alt, long alt_num,
                                                 size_t volta_depth,
                                                 SCM volta_nums)
    = 0;
  virtual void derived_report_return (long alt_num, long return_count) = 0;
  virtual void derived_report_alternative_group_end (Music *element,
                                                     size_t volta_depth)
    = 0;

private:
  // Think twice before adding state or logic to the repeat stylers.  Dealing
  // with the complexities of music structure should mainly be left to the
  // music iterators.  The stylers should mainly just act on what the iterators
  // have discovered, using information that is plainly communicated to them.
  Music_iterator *owner_ = nullptr;
  Interval_t<Moment> spanned_time_ {Moment::infinity (), Moment::infinity ()};
  long repeat_count_ = 2;
  size_t alternative_depth_ = 0;
  size_t reported_return_depth_ = 0;
  bool reported_return_ = false;
};

#endif // REPEAT_STYLER_HH
