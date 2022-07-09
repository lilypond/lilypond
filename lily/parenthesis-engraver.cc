/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>


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
  // When we see parenthesis-id set, we make a single Parentheses grob
  // for all grobs having the same value.  This alist maps IDs (symbols)
  // to Parentheses grobs.  It is reset after each time step.
  SCM id_alist_ = SCM_EOL;
  void derived_mark () const override;
  void acknowledge_grob (Grob_info) override;
  void stop_translation_timestep ();
};

Parenthesis_engraver::Parenthesis_engraver (Context *c)
  : Engraver (c)
{
}

void
Parenthesis_engraver::derived_mark () const
{
  scm_gc_mark (id_alist_);
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
      SCM id = get_property (g, "parenthesis-id");
      bool must_add_to_alist = false;
      Grob *paren = nullptr;
      if (scm_is_symbol (id))
        {
          SCM maybe_paren = scm_assq_ref (id_alist_, id);
          if (scm_is_true (maybe_paren))
            paren = unsmob<Grob> (maybe_paren);
          else
            must_add_to_alist = true;
        }
      if (!paren)
        {
          Engraver *const eng = info.origin_engraver ();
          paren = eng->make_sticky ("Parentheses", g, g->self_scm ());
        }
      if (must_add_to_alist)
        {
          // No need for scm_assq_set_x: we already know that the
          // id is not a key in the alist.
          id_alist_ = scm_acons (id, paren->self_scm (), id_alist_);
        }

      Pointer_group_interface::add_grob (paren, ly_symbol2scm ("elements"), g);

      Real size = from_scm<double> (get_property (paren, "font-size"), 0.0)
                  + from_scm<double> (get_property (g, "font-size"), 0.0);
      set_property (paren, "font-size", to_scm (size));

      /*
        TODO?

        enlarge victim to allow for parentheses space?
      */
    }
}

void
Parenthesis_engraver::stop_translation_timestep ()
{
  id_alist_ = SCM_EOL;
}

void
Parenthesis_engraver::boot ()
{
  ADD_ACKNOWLEDGER (grob);
}

ADD_TRANSLATOR (Parenthesis_engraver,
                /* doc */
                R"(
Parenthesize objects whose @code{parenthesize} property is @code{#t}.
                )",

                /* create */
                R"(
Parentheses
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
