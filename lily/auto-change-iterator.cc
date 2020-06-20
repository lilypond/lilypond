/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "change-iterator.hh"
#include "change-sequence-iterator.hh"

using std::string;

class Auto_change_iterator final : public Change_sequence_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Auto_change_iterator () {}

private:
  void change_to (const string &id) override;
};

void
Auto_change_iterator::change_to (const string &id)
{
  // N.B. change_to() returns an error message. Silence is the legacy
  // behavior here, but maybe that should be changed.
  Change_iterator::change_to (*child_iter_, ly_symbol2scm ("Staff"), id);
}

IMPLEMENT_CTOR_CALLBACK (Auto_change_iterator);
