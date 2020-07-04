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

#include "callback.hh"
#include "context.hh"
#include "input.hh"
#include "repeated-music.hh"
#include "sequential-iterator.hh"
#include "lily-imports.hh"

class Percent_repeat_iterator final : public Sequential_iterator
{
public:
  OVERRIDE_CLASS_NAME (Percent_repeat_iterator);
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Percent_repeat_iterator ();
protected:
  SCM get_music_list () const override;
  void create_children () override;
private:
  SCM music_tail () const;
  int starting_bar_;
};

IMPLEMENT_CTOR_CALLBACK (Percent_repeat_iterator);

Percent_repeat_iterator::Percent_repeat_iterator ()
  : starting_bar_ (-1)
{
}

void
Percent_repeat_iterator::create_children ()
{
  Sequential_iterator::create_children ();

  descend_to_bottom_context ();
  if (!measure_position (get_context ()).main_part_)
    starting_bar_
      = from_scm (get_property (get_context (), "internalBarNumber"), 0);
}

// Todo: use elements-callback instead?  We don't expose iterator
// member functions elsewhere, though.
SCM
Percent_repeat_iterator::get_music_list () const
{
  return scm_cons (Repeated_music::body (get_music ())->self_scm (),
                   MFP_WRAP (&Percent_repeat_iterator::music_tail));
}

// Arrive here when the original percent expression has been
// completed.  At this point of time, we can determine what kind of
// percent expression we are dealing with and provide the respective
// music expressions for the remaining repeats.
SCM
Percent_repeat_iterator::music_tail () const
{
  Music *mus = get_music ();

  Music *child = Repeated_music::body (mus);
  SCM length = child->get_length ().smobbed_copy ();

  int current_bar = -1;
  if (!measure_position (get_context ()).main_part_)
    current_bar
      = from_scm (get_property (get_context (), "internalBarNumber"), 0);

  SCM child_list = SCM_EOL;

  SCM event_type = SCM_UNDEFINED;
  SCM slash_count = SCM_UNDEFINED; // Only defined for RepeatSlashEvent

  if (starting_bar_ >= 0 && current_bar == starting_bar_ + 1)
    event_type = ly_symbol2scm ("PercentEvent");
  else if (starting_bar_ >= 0 && current_bar == starting_bar_ + 2)
    event_type = ly_symbol2scm ("DoublePercentEvent");
  else
    {
      slash_count = Lily::calc_repeat_slash_count (child->self_scm ());
      event_type = ly_symbol2scm ("RepeatSlashEvent");
    }

  int repeats = scm_to_int (get_property (mus, "repeat-count"));
  for (int i = repeats; i > 1; i--)
    {
      Music *percent = make_music_by_name (event_type);
      percent->set_spot (*mus->origin ());
      set_property (percent, "length", length);
      if (repeats > 1)
        {
          set_property (percent, "repeat-count", to_scm (i));
          if (!SCM_UNBNDP (slash_count))
            set_property (percent, "slash-count", slash_count);
        }

      child_list = scm_cons (percent->unprotect (), child_list);
    }

  return child_list;
}
