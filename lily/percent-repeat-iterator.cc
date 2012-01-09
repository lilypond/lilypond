/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2012  Han-Wen Nienhuys <hanwen@xs4all.nl>
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

class Percent_repeat_iterator : public Sequential_iterator
{
public:
  DECLARE_CLASSNAME (Percent_repeat_iterator);
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Percent_repeat_iterator ();
protected:
  virtual SCM get_music_list () const;
};

IMPLEMENT_CTOR_CALLBACK (Percent_repeat_iterator);

Percent_repeat_iterator::Percent_repeat_iterator ()
{
}

SCM
Percent_repeat_iterator::get_music_list () const
{
  Music *mus = get_music ();
  Music *child = Repeated_music::body (mus);
  SCM length = child->get_length ().smobbed_copy ();
  SCM child_list = SCM_EOL;
  Moment measure_len = measure_length (get_outlet ());
  Moment music_len = robust_scm2moment (length, Moment (0));

  string event_type;
  SCM slash_count = SCM_EOL;

  if (measure_len == music_len)
    event_type = "PercentEvent";
  else if (measure_len * Moment (2) == music_len)
    event_type = "DoublePercentEvent";
  else
    {
      slash_count
        = scm_call_1 (ly_lily_module_constant ("calc-repeat-slash-count"),
                      child->self_scm ());
      event_type = "RepeatSlashEvent";
    }

  int repeats = scm_to_int (mus->get_property ("repeat-count"));
  for (int i = repeats; i > 1; i--)
    {
      Music *percent = make_music_by_name (ly_symbol2scm (event_type.c_str ()));
      percent->set_spot (*mus->origin ());
      percent->set_property ("length", length);
      if (repeats > 1)
        {
          percent->set_property ("repeat-count", scm_from_int (i));
          if (event_type == "RepeatSlashEvent")
            percent->set_property ("slash-count", slash_count);
        }

      child_list = scm_cons (percent->unprotect (), child_list);
    }

  child_list = scm_cons (child->self_scm (), child_list);

  return child_list;
}
