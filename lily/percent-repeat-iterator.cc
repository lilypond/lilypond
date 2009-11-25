/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2009  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  /* TODO: Distinction between percent, double-percent and slash */
  Music *mus = get_music ();
  Music *child = Repeated_music::body (mus);
  SCM length = child->get_length ().smobbed_copy ();
  SCM child_list = SCM_EOL;

  int repeats = scm_to_int (mus->get_property ("repeat-count"));
  for (int i = repeats; i > 1; i--)
  {
    Music *percent = make_music_by_name (ly_symbol2scm ("PercentEvent"));
    percent->set_spot (*mus->origin ());
    percent->set_property ("length", length);
    if (repeats > 1)
      percent->set_property ("repeat-count", scm_int2num (i));
    
    child_list = scm_cons (percent->unprotect (), child_list);
  }
  
  child_list = scm_cons (child->self_scm (), child_list);

  return child_list;
}
