/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "engraver.hh"

#include "international.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Parenthesis_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Parenthesis_engraver);

protected:
  void acknowledge_grob (Grob_info) override;
};

Parenthesis_engraver::Parenthesis_engraver (Context *c)
  : Engraver (c)
{
}

void
Parenthesis_engraver::acknowledge_grob (Grob_info info)
{
  if (Stream_event *ev = info.event_cause ())
    {
      if (from_scm<bool> (get_property (ev, "parenthesize")))
        {
          if (Item *victim = dynamic_cast<Item *> (info.grob ()))
            {
              Engraver *eng = dynamic_cast<Engraver *> (info.origin_translator ());
              Item *paren = eng->make_item ("ParenthesesItem", victim->self_scm ());
              Pointer_group_interface::add_grob (paren, ly_symbol2scm ("elements"), victim);

              paren->set_y_parent (victim);

              Real size = from_scm<double> (get_property (paren, "font-size"), 0.0)
                          + from_scm<double> (get_property (victim, "font-size"), 0.0);
              set_property (paren, "font-size", to_scm (size));

              /*
                TODO?

                enlarge victim to allow for parentheses space?
              */
            }
          else
            {
              info.grob ()->warning ("Don't know how to parenthesize spanners.");
            }
        }
    }
}

void
Parenthesis_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Parenthesis_engraver, grob);
}

ADD_TRANSLATOR (Parenthesis_engraver,
                /* doc */
                "Parenthesize objects whose music cause has the"
                " @code{parenthesize} property.",

                /* create */
                "ParenthesesItem ",

                /* read */
                "",

                /* write */
                ""
               );
