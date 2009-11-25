/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2009 Erik Sandberg  <mandolaerik@gmail.com>

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

#include "scheme-listener.hh"

IMPLEMENT_LISTENER (Scheme_listener, call)
void
Scheme_listener::call (SCM ev)
{
  scm_call_1 (callback_, ev);
}

IMPLEMENT_SMOBS (Scheme_listener);
IMPLEMENT_DEFAULT_EQUAL_P (Scheme_listener);

Scheme_listener::Scheme_listener (SCM c)
{
  callback_ = SCM_EOL;
  self_scm_ = SCM_EOL;
  smobify_self ();
  callback_ = c; 
}

SCM
Scheme_listener::mark_smob (SCM obj)
{
  Scheme_listener *me = (Scheme_listener *) SCM_CELL_WORD_1 (obj);
  return me->callback_;
}

int
Scheme_listener::print_smob (SCM obj, SCM p, scm_print_state*)
{
  Scheme_listener *me = (Scheme_listener *) SCM_CELL_WORD_1 (obj);
  scm_puts ("#<Scheme_listener ", p);
  scm_write (me->callback_, p);
  scm_puts (">", p);
  return 1;
}

Scheme_listener::~Scheme_listener ()
{
}
