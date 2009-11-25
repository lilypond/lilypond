/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "international.hh"
#include "music.hh"
#include "simple-music-iterator.hh"

class Apply_context_iterator : public Simple_music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual void process (Moment);
};

void
Apply_context_iterator::process (Moment m)
{
  SCM proc = get_music ()->get_property ("procedure");

  if (ly_is_procedure (proc))
    scm_call_1 (proc, get_outlet ()->self_scm ());
  else
    get_music ()->origin ()->warning (_ ("\\applycontext argument is not a procedure"));

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Apply_context_iterator);

