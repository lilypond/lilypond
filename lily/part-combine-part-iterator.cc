/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2015--2020 Daniel Eble <dan@faithful.be>

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

#include "change-sequence-iterator.hh"
#include "context.hh"

using std::string;

class Part_combine_part_iterator final : public Change_sequence_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Part_combine_part_iterator () {}

private:
  void change_to (const string &id) override;
  Context *find_voice (const string &id);
};

void
Part_combine_part_iterator::change_to (const string &id)
{
  Context *voice = find_voice (id);
  if (voice)
    substitute_outlet (get_outlet (), voice);
  else
    {
      string s = "can not find Voice context: ";
      s += id;
      programming_error (s);
    }
}

Context *
Part_combine_part_iterator::find_voice (const string &id)
{
  // Find a Voice among the siblings of the current outlet.  (Well, this might
  // also find a sibling's descendant, but that should not be a problem.)
  Context *c = get_outlet ()->get_parent_context ();
  if (c)
    return find_context_below (c, ly_symbol2scm ("Voice"), id);
  programming_error ("no parent context");
  return 0;
}

IMPLEMENT_CTOR_CALLBACK (Part_combine_part_iterator);
