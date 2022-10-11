/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2003--2022 Juergen Reuter <reuter@ipd.uka.de>

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

#include "warn.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"

using std::vector;

/*
 * This abstract class serves as common superclass for all ligature
 * engravers thet produce a single connected graphical object of fixed
 * width, consisting of noteheads and other primitives (see class
 * Ligature_engraver for more information on the interaction between
 * this class and its superclass).  In particular, it cares for the
 * following tasks:
 *
 * - provide a function for putting all grobs of the ligature into a
 * single paper column,
 *
 * - delegate actual creation of ligature to concrete subclass,
 *
 * - except in Kievan notation, collect all accidentals that occur
 * within the ligature and put them at the left side of the ligature
 * (TODO; see function collect_accidentals ()),
 *
 * - collapse superflous space after each ligature (TODO).
 *
 * Concrete subclasses must implement function build_ligature (Spanner *,
 * vector<Grob_info_t<Item>>).  This function is responsible for actually
 * building the ligature by transforming the array of noteheads.
 *
 * Currently, there are two subclasses: Gregorian_ligature_engraver
 * for Gregorian chant notation (also known as plain song or cantus
 * planus) and Mensural_ligature_engraver for white mensural notation.
 * Subclasses for other music notation styles such as modal notation
 * or ars nova notation may eventually be added.
 */

/*
 * TODO: local accidentals: collect accidentals that occur within a
 * ligature and put them before the ligature.  If an accidental
 * changes within a ligature, print a warning (user error) and ignore
 * any further accidental for that pitch within that ligature
 * (actually, in such a case, the user should split the ligature into
 * two separate ligatures).  Similarly, any object that, in ordinary
 * notation, may be put to the left or to the right of a note-head,
 * should be collected and put before or after the ligature.
 *
 * TODO: make spacing more robust: do not screw up spacing if user
 * erroneously puts rest in ligature.
 *
 * TODO: for each ligature, add Rod that represents the total length
 * of the ligature (to preemptively avoid collision with adjacent
 * notes); or maybe just additionally create a
 * mensural/vaticana/whatever-ligature grob (e.g. via
 * Mensural_ligature::print (SCM)) that just consists of a
 * bounding box around all primitives of the ligature.
 *
 * TODO: Maybe move functions fold_up_primitives () and
 * join_primitives () from subclasses to here?  N.B. it is not
 * appropriate to put these into Ligature_engraver, since, for
 * example, Ligature_bracket_engraver does not share any of this code.
 */

/*
  FIXME: this is very ugly.  Instead of moving items around between columns,
  we should set exact spacing constraints between these columns.

  It also changes some grob relationships by setting the parent of all these
  grobs to the paper column, e.g. disconnecting Dots from their DotColumn.
  This is actually relied upon by the engravers.
 */
void
Coherent_ligature_engraver::move_related_items_to_column (
  Item *item, Paper_column *target_column, Real offset)
{
  Paper_column *source_column = item->get_column ();
  Grob *staff_symbol = Staff_symbol_referencer::get_staff_symbol (item);
  extract_item_set (source_column, "elements", elements);
  for (vsize i = elements.size (); i--;)
    {
      Item *sibling = elements[i];
      if (!sibling)
        // should not occur, but who knows... -jr
        continue;

      if (Staff_symbol_referencer::get_staff_symbol (sibling) != staff_symbol)
        // sibling is from a staff different than that of the item of
        // interest
        continue;

#if 0 /* experimental code to collapse spacing after ligature */
      Grob *sibling_parent = sibling->get_x_parent ();
      sibling_parent->warning (_f ("Coherent_ligature_engraver: "
                                   "setting `spacing-increment="
                                   "0.01': ptr=%ul", parent));
      set_property (sibling_parent, "forced-spacing",
                    to_scm (0.01));
#endif

      sibling->set_x_parent (target_column);
      sibling->translate_axis (offset, X_AXIS);
    }
}

/*
 * TODO: This function should collect all accidentals that occur
 * within the ligature (by scanning through the primitives array) and
 * place all of them at the left of the ligature.  If there is an
 * alteration within the ligature (e.g. an "f" followed by a "fis"
 * somewhere later in the ligature), issue a warning (and maybe create
 * an additional natural symbol to explicitly make clear that there is
 * an "f" first?).  The warning should suggest the user to break the
 * ligature into two or more smaller ligatures such that no alteration
 * occurs within the broken ligatures any more.
 */
void
Coherent_ligature_engraver::collect_accidentals (Spanner *,
                                                 vector<Item *> const &)
{
  /* TODO */
  /* NOTE: if implementing such a function, note that in Kievan notation,
   * the B-flat accidental should not be "collected", but rather prints
   * immediately before the note head as usual. */
}

void
calc_delta_pitches (vector<Item *> const &primitives)
{
  if (primitives.empty ())
    return;

  auto prev_pitch
    = unsmob<Pitch> (get_property (primitives[0]->event_cause (), "pitch"))
        ->steps ();
  for (vsize i = 1; i < primitives.size (); ++i)
    {
      auto *const cause = primitives[i]->event_cause ();
      auto pitch = unsmob<Pitch> (get_property (cause, "pitch"))->steps ();

      auto *const prev_item = primitives[i - 1];
      set_property (prev_item, "delta-position", to_scm (pitch - prev_pitch));

      prev_pitch = pitch;
    }

  set_property (primitives.back (), "delta-position", to_scm (0));
}

void
Coherent_ligature_engraver::typeset_ligature (Spanner *ligature,
                                              vector<Item *> const &primitives)
{
  // compute some commonly needed context info stored as grob
  // properties
  calc_delta_pitches (primitives);

  // prepare ligature for typesetting
  build_ligature (ligature, primitives);
  collect_accidentals (ligature, primitives);
}

// no ADD_ACKNOWLEDGER / ADD_ACKNOWLEDGER / ADD_TRANSLATOR macro calls
// since this class is abstract
