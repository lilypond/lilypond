/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music.hh"
#include "sequential-iterator.hh"
#include "context.hh"

class Unfolded_repeat_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual SCM get_music_list () const;
};

SCM
Unfolded_repeat_iterator::get_music_list () const
{
  SCM l = SCM_EOL;
  SCM *tail = &l;

  SCM body = get_music ()->get_property ("element");
  SCM alts = get_music ()->get_property ("elements");
  int alt_count = scm_ilength (alts);
  int rep_count = scm_to_int (get_music ()->get_property ("repeat-count"));

  for (int i = 0; i < rep_count; i++)
    {
      if (unsmob_music (body))
	*tail = scm_cons (body, SCM_EOL);

      tail = SCM_CDRLOC (*tail);

      if (alt_count)
	{
	  *tail = scm_cons (scm_car (alts), SCM_EOL);
	  tail = SCM_CDRLOC (*tail);
	  if (i >= rep_count - alt_count)

	    alts = scm_cdr (alts);
	}
    }

  return l;
}

IMPLEMENT_CTOR_CALLBACK (Unfolded_repeat_iterator);
