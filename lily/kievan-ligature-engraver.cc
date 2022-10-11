/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2013--2022 Aleksandr Andreev <aleksandr.andreev@gmail.com>

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

#include "coherent-ligature-engraver.hh"
#include "font-interface.hh"
#include "international.hh"
#include "kievan-ligature.hh"
#include "paper-column.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

class Kievan_ligature_engraver : public Coherent_ligature_engraver
{

protected:
  Spanner *create_ligature_spanner () override;
  void build_ligature (Spanner *ligature,
                       vector<Item *> const &primitives) override;

public:
  TRANSLATOR_DECLARATIONS (Kievan_ligature_engraver);

private:
  void fold_up_primitives (vector<Item *> const &primitives, Real padding,
                           Real &min_length);
};

Kievan_ligature_engraver::Kievan_ligature_engraver (Context *c)
  : Coherent_ligature_engraver (c)
{
}

Spanner *
Kievan_ligature_engraver::create_ligature_spanner ()
{
  return make_spanner ("KievanLigature", SCM_EOL);
}

void
Kievan_ligature_engraver::fold_up_primitives (vector<Item *> const &primitives,
                                              Real padding, Real &min_length)
{
  Item *first = 0;
  Real accumul_acc_space = 0.0;
  // start us off with some padding on the left
  min_length = padding;

  for (vsize i = 0; i < primitives.size (); i++)
    {
      auto *const current = primitives[i];
      Interval my_ext = current->extent (current, X_AXIS);
      Real head_width = my_ext.length ();
      if (i == 0)
        first = current;

      // must keep track of accidentals in spacing problem
      Grob *acc_gr = unsmob<Grob> (get_object (current, "accidental-grob"));
      if (acc_gr && i > 0)
        {
          Interval acc_ext = acc_gr->extent (acc_gr, X_AXIS);
          accumul_acc_space += acc_ext.length ();
        }

      move_related_items_to_column (current, first->get_column (), min_length);

      // check if we have any dots
      if (size_t const dot_count = Rhythmic_head::dot_count (current))
        {
          Grob *dot_gr = Rhythmic_head::get_dots (current);

          /*
            This is ugly and should probably be handled by configuring
            the DotColumn appropriately.  Note that these dots will
            be disconnected from their dot column.  See
            move_related_items_to_column.

            This also means the padding isn't configurable as DotColumn.padding is.
          */
          const Stencil *stil
            = unsmob<const Stencil> (get_property (dot_gr, "dot-stencil"));
          Real dot_width = stil ? stil->extent (X_AXIS).length () : 0.0;
          head_width += dot_width - 0.5 * (padding - accumul_acc_space);

          dot_gr->translate_axis (
            0.5 * (padding - accumul_acc_space) + dot_width, X_AXIS);
        }

      // add more padding if we have an accidental coming up
      if (i < primitives.size () - 1)
        {
          auto *const next = primitives[i + 1];
          Grob *acc_gr = unsmob<Grob> (get_object (next, "accidental-grob"));
          if (acc_gr)
            {
              Interval acc_ext = acc_gr->extent (acc_gr, X_AXIS);
              padding += acc_ext.length ();
            }
        }

      min_length += head_width + padding - accumul_acc_space;
    }
}

void
Kievan_ligature_engraver::build_ligature (Spanner *ligature,
                                          vector<Item *> const &primitives)
{
  Real min_length;

  Real padding = from_scm<double> (get_property (ligature, "padding"), 0.0);
  fold_up_primitives (primitives, padding, min_length);
  if (from_scm<double> (get_property (ligature, "minimum-length"), 0.0)
      < min_length)
    set_property (ligature, "minimum-length", to_scm (min_length));
}

void
Kievan_ligature_engraver::boot ()
{
  ADD_DELEGATE_LISTENER (ligature);
  ADD_ACKNOWLEDGER (rest);
  ADD_ACKNOWLEDGER (ligature_head);
}

ADD_TRANSLATOR (Kievan_ligature_engraver,
                /* doc */
                R"(
Handle @code{Kievan_ligature_events} by glueing Kievan heads together.
                )",

                /* create */
                R"(
KievanLigature
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
