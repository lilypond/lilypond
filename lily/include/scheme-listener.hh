/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2010 Erik Sandberg  <mandolaerik@gmail.com>

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

#ifndef SCHEME_LISTENER_HH
#define SCHEME_LISTENER_HH

#include "listener.hh"
#include "ly-smobs.icc"

/*
 Scheme_listener is only used internally by scheme-listener-scheme.cc
*/

class Scheme_listener
{
public:
  Scheme_listener (SCM callback);
  DECLARE_LISTENER (call);
protected:
  DECLARE_SMOBS (Scheme_listener);
private:
  SCM callback_;
};

#endif /* SCHEME_LISTENER_HH */
