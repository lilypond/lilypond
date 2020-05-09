/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>
                  Erik Sandberg <mandolaerik@gmail.com>

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

#include "context.hh"
#include "input.hh"
#include "repeated-music.hh"
#include "sequential-iterator.hh"
#include "lily-imports.hh"

using std::string;

class Percent_repeat_iterator : public Sequential_iterator
{
public:
  OVERRIDE_CLASS_NAME (Percent_repeat_iterator);
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Percent_repeat_iterator ();
protected:
  SCM get_music_list () const override;
  void construct_children () override;
  void next_element () override;
  void derived_mark () const override;
private:
  SCM child_list_;
  int starting_bar_;
};

IMPLEMENT_CTOR_CALLBACK (Percent_repeat_iterator);

Percent_repeat_iterator::Percent_repeat_iterator ()
  : child_list_ (SCM_UNDEFINED), starting_bar_ (-1)
{
}

void
Percent_repeat_iterator::derived_mark () const
{
  scm_gc_mark (child_list_);
  Sequential_iterator::derived_mark ();
}

void
Percent_repeat_iterator::construct_children ()
{
  Music *mus = get_music ();

  Music *child = Repeated_music::body (mus);
  child_list_ = scm_list_1 (child->self_scm ());

  Sequential_iterator::construct_children ();

  descend_to_bottom_context ();
  if (!measure_position (get_outlet ()).main_part_)
    starting_bar_
      = robust_scm2int (get_property (get_outlet (), "internalBarNumber"), 0);
}

SCM
Percent_repeat_iterator::get_music_list () const
{
  return child_list_;
}

// Arrive here when child processing has been completed.  At that
// point of time, we can determine what kind of percent expression we
// are dealing with and extend the child list just in time before the
// next iterator is getting fetched.
void
Percent_repeat_iterator::next_element ()
{
  // Do nothing if we already did our processing here.
  if (!scm_is_pair (child_list_))
    {
      Sequential_iterator::next_element ();
      return;
    }

  Music *mus = get_music ();

  Music *child = Repeated_music::body (mus);
  SCM length = child->get_length ().smobbed_copy ();

  int current_bar = -1;
  if (!measure_position (get_outlet ()).main_part_)
    current_bar
      = robust_scm2int (get_property (get_outlet (), "internalBarNumber"), 0);

  SCM child_list = SCM_EOL;

  string event_type;
  SCM slash_count = SCM_EOL;

  if (starting_bar_ >= 0 && current_bar == starting_bar_ + 1)
    event_type = "PercentEvent";
  else if (starting_bar_ >= 0 && current_bar == starting_bar_ + 2)
    event_type = "DoublePercentEvent";
  else
    {
      slash_count = Lily::calc_repeat_slash_count (child->self_scm ());
      event_type = "RepeatSlashEvent";
    }

  int repeats = scm_to_int (get_property (mus, "repeat-count"));
  for (int i = repeats; i > 1; i--)
    {
      Music *percent = make_music_by_name (ly_symbol2scm (event_type.c_str ()));
      percent->set_spot (*mus->origin ());
      set_property (percent, "length", length);
      if (repeats > 1)
        {
          set_property (percent, "repeat-count", scm_from_int (i));
          if (event_type == "RepeatSlashEvent")
            set_property (percent, "slash-count", slash_count);
        }

      child_list = scm_cons (percent->unprotect (), child_list);
    }

  scm_set_cdr_x (child_list_, child_list);
  child_list_ = SCM_EOL;

  Sequential_iterator::next_element ();
}
