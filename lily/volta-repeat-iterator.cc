/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "lily-imports.hh"
#include "music.hh"

using std::string;

class Volta_repeat_iterator final : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Volta_repeat_iterator () = default;

  void add_repeat_command (SCM);
protected:
  void process (Moment) override;

private:
  bool empty () const
  {
    return !music_get_length () && !music_start_mom ().grace_part_;
  }

private:
  bool started_ = false;
  bool stopped_ = false;
};

/*
  TODO: add source information for debugging
*/
void
Volta_repeat_iterator::add_repeat_command (SCM what)
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
Volta_repeat_iterator::process (Moment m)
{
  if (!started_)
    {
      // Robustness: Avoid printing a misleading bar line for a zero-duration
      // repeated section.
      if (!empty ())
        add_repeat_command (ly_symbol2scm ("start-repeat"));
      started_ = true;
    }

  Sequential_iterator::process (m);

  if (started_ && !stopped_ && (m == music_get_length ()))
    {
      if (!empty ())
        {
          // When there are tail alternatives, Alternative_sequence_iterator
          // issues end-repeat commands.
          //
          // TODO: The 'elements property informs us of this:
          //
          //     \repeat { ... } \alternative { ... }
          //
          // but not this:
          //
          //     \repeat { ... \alternative { ... } }
          //
          bool has_alts = scm_is_pair (get_property (get_music (), "elements"));
          if (!has_alts)
            add_repeat_command (ly_symbol2scm ("end-repeat"));
        }
      stopped_ = true;
    }
}

IMPLEMENT_CTOR_CALLBACK (Volta_repeat_iterator);
