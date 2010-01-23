/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef CONTEXT_HANDLE_HH
#define CONTEXT_HANDLE_HH

#include "lily-proto.hh"


class Context_handle
{
public:
  ~Context_handle ();
  Context_handle ();

  void set_context (Context *);
  void operator = (Context_handle const &);
  Context_handle (Context_handle const &);
  Context *get_outlet () const;

  int get_count () const;
private:
  Context *outlet_;
  void down ();
  void up (Context *);
};

#endif /* CONTEXT_HANDLE_HH */

