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
  void derived_report_alternative_start (Music *, long, SCM) override {}
  void derived_report_return (long /*alt_num*/) override {}
  void derived_report_alternative_group_end (Music *) override {}
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

  void report_event (Music *element, Direction d, SCM volta_nums)
  {
    SCM ev_scm = Lily::make_music (ly_symbol2scm ("AlternativeEvent"));
    auto *const ev = unsmob<Music> (ev_scm);
    if (element)
      {
        if (auto *origin = element->origin ())
          ev->set_spot (*origin);
      }
    set_property (ev, "alternative-dir", to_scm (d));
    set_property (ev, "volta-numbers", volta_nums);
    ev->send_to_context (owner ()->get_context ());
  }

public:
  explicit Volta_repeat_styler (Music_iterator *owner) : Repeat_styler (owner)
  {
  }

  void derived_report_start () override
  {
    add_repeat_command (ly_symbol2scm ("start-repeat"));
  }

  void derived_report_alternative_start (Music *element,
                                         long alt_num, SCM volta_nums) override
  {
    report_event (element, (alt_num == 1) ? START : CENTER, volta_nums);
  }

  void derived_report_return (long /*alt_num*/) override
  {
    add_repeat_command (ly_symbol2scm ("end-repeat"));
  }

  void derived_report_alternative_group_end (Music *element) override
  {
    report_event (element, STOP, SCM_EOL);
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
