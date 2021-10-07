/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "repeat-styler.hh"

#include "context.hh"
#include "input.hh"
#include "lily-imports.hh"
#include "music.hh"
#include "music-iterator.hh"

#include <string>

class Null_repeat_styler final : public Repeat_styler
{
public:
  explicit Null_repeat_styler (Music_iterator *owner) : Repeat_styler (owner)
  {
  }

  void derived_report_start () override {}
  void derived_report_end () override {}
};

class Volta_repeat_styler final : public Repeat_styler
{
private:
  void add_repeat_command (SCM new_cmd) const
  {
    SCM sym = ly_symbol2scm ("repeatCommands");
    SCM cmds = SCM_EOL;
    auto *where = where_defined (owner ()->get_context (), sym, &cmds);

    if (where && ly_cheap_is_list (cmds))
      {
        cmds = scm_cons (new_cmd, cmds);
        set_property (where, sym, cmds);
      }
  }

public:
  explicit Volta_repeat_styler (Music_iterator *owner) : Repeat_styler (owner)
  {
  }

  void derived_report_start () override
  {
    add_repeat_command (ly_symbol2scm ("start-repeat"));
  }

  void derived_report_end () override
  {
    add_repeat_command (ly_symbol2scm ("end-repeat"));
  }
};

std::unique_ptr<Repeat_styler>
Repeat_styler::create_null (Music_iterator *owner)
{
  return std::unique_ptr<Repeat_styler> (new Null_repeat_styler (owner));
}

std::unique_ptr<Repeat_styler>
Repeat_styler::create_volta (Music_iterator *owner)
{
  return std::unique_ptr<Repeat_styler> (new Volta_repeat_styler (owner));
}
