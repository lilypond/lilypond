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

LY_DEFINE (ly_make_listener, "ly:make-listener",
	   1, 0, 0, (SCM callback),
	   "Create a listener.  Any time the listener hears an object,"
	   " it will call @var{callback} with that object.\n"
	   "\n"
	   "@var{callback} should take exactly one argument.")
{
  LY_ASSERT_TYPE (ly_is_procedure, callback, 1);
  Scheme_listener *l = new Scheme_listener (callback);
  SCM listener = GET_LISTENER (l->call).smobbed_copy ();
  l->unprotect ();
  return listener;
}
