/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2015--2021 Daniel Eble <nine.fierce.ballads@gmail.com>

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

class Part_combine_part_iterator final : public Change_sequence_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Part_combine_part_iterator () {}

private:
  void change_to (const string &id) override;
};

void
Part_combine_part_iterator::change_to (const string &id)
{
  Change_iterator::change_to (*get_child (), ly_symbol2scm ("Voice"), id);
}

IMPLEMENT_CTOR_CALLBACK (Part_combine_part_iterator);
