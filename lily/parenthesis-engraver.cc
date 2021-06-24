/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "accidental-interface.hh"
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
  Grob *g = info.grob ();
  if (from_scm<bool> (get_property (g, "parenthesized"))
      // AccidentalCautionary has its own implementation
      // of parentheses.  It changes the stencil, which
      // is important for accidental placement, but won't
      // work with parenthesis friends.  TODO: find a nice
      // way to merge the two.
      && !has_interface<Accidental_interface> (g))
    {
      if (Item *victim = dynamic_cast<Item *> (g))
        {
          auto *const eng = info.origin_engraver ();
          Item *paren = eng->make_item ("Parentheses", victim->self_scm ());
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
          info.grob ()->warning (_ ("Don't know how to parenthesize spanners."));
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
                "Parenthesize objects whose @code{parenthesize} property"
                " is @code{#t}.",

                /* create */
                "Parentheses ",

                /* read */
                "",

                /* write */
                ""
               );
