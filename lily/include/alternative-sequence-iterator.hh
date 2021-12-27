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

#include "sequential-iterator.hh"

#include "lily-guile-macros.hh"

#include <memory>

class Repeat_styler;

// iterator for \alternative {...}
class Alternative_sequence_iterator final : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Alternative_sequence_iterator () = default;

  // The number of enclosing \alternatives with volta brackets enabled (not
  // limited to the current \repeat, all the way to the root of the music),
  // plus one if volta brackets are enabled for this \alternative.
  // N.B. Before the first call to process (), the result is possibly
  // incorrect.
  size_t volta_bracket_depth () const { return volta_bracket_depth_; }

  // True if volta brackets should be created for this group of alternatives.
  // N.B. Before the first call to process (), the result is possibly
  // incorrect.
  bool volta_brackets_enabled () const { return volta_brackets_enabled_; }

protected:
  void next_element () override;
  void create_children () override;
  void process (Moment) override;
  void derived_mark () const override;

  void end_alternative ();
  void restore_context_properties ();
  void save_context_properties ();
  void start_alternative ();

private:
  void analyze ();

private:
  struct Alternative_info
  {
    size_t return_count = 0;
  };

  bool first_time_ = true;
  bool volta_brackets_enabled_ = true;
  size_t volta_bracket_depth_ = 1;
  size_t done_count_ = 0;
  SCM alt_restores_ = SCM_EOL;
  std::shared_ptr<Repeat_styler> repeat_styler_;
  std::vector<Alternative_info> alt_info_;
};
