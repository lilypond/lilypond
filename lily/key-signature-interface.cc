/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  keyplacement by Mats Bengtsson

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

#include "accidental-interface.hh"
#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "rational.hh"
#include "lily-imports.hh"

using std::string;

struct Key_signature_interface
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

/*
  TODO
  - space the `natural' signs wider
*/
MAKE_SCHEME_CALLBACK (Key_signature_interface, print,
                      "ly:key-signature-interface::print", 1);
SCM
Key_signature_interface::print (SCM smob)
{
  Item *me = unsmob<Item> (smob);

  Real inter = Staff_symbol_referencer::staff_space (me) / 2.0;

  Stencil mol;

  SCM c0s = get_property (me, "c0-position");

  bool is_cancellation
    = me->internal_has_interface (ly_symbol2scm ("key-cancellation-interface"));

  /*
    SCM lists are stacks, so we work from right to left, ending with
    the cancellation signature.
  */

  Slice ht_right, last_ht_left; /* ht intervals for natural glyph kerning */
  SCM last_glyph_name = SCM_BOOL_F;
  SCM padding_pairs = get_property (me, "padding-pairs");

  Font_metric *fm = Font_interface::get_default_font (me);
  SCM props = Font_interface::music_font_alist_chain (me);
  SCM alist = ly_chain_assoc_get (ly_symbol2scm ("alteration-glyph-name-alist"),
                                  props, SCM_EOL);

  for (SCM s = get_property (me, "alteration-alist"); scm_is_pair (s);
       s = scm_cdr (s))
    {
      SCM alt = is_cancellation ? to_scm (0) : scm_cdar (s);

      SCM glyph_name_scm = ly_assoc_get (alt, alist, SCM_BOOL_F);
      if (!scm_is_string (glyph_name_scm))
        {
          me->warning (_f ("No glyph found for alteration: %s",
                           from_scm<Rational> (alt).to_string ().c_str ()));
          continue;
        }

      string glyph_name = ly_scm2string (glyph_name_scm);

      Stencil acc (fm->find_by_name (glyph_name));

      if (acc.is_empty ())
        me->warning (_ ("alteration not found"));
      else
        {
          ht_right.set_empty ();
          Stencil column;
          for (SCM pos_list
               = Lily::key_signature_interface_alteration_positions (
                 scm_car (s), c0s, smob);
               scm_is_pair (pos_list); pos_list = scm_cdr (pos_list))
            {
              int p = from_scm<int> (scm_car (pos_list));
              ht_right.add_point (2 * p - 6); /* descender */
              ht_right.add_point (2 * p + 3); /* upper right corner */
              column.add_stencil (acc.translated (Offset (0, p * inter)));
            }
          /*
            The natural sign (unlike flat & sharp)
            has vertical edges on both sides. A little padding is
            needed to prevent collisions.
          */
          Real padding = from_scm<double> (get_property (me, "padding"), 0.0);
          SCM handle = ly_assoc (scm_cons (glyph_name_scm, last_glyph_name),
                                 padding_pairs);
          if (scm_is_pair (handle))
            padding = from_scm<double> (scm_cdr (handle), 0.0);
          else if (glyph_name == "accidentals.natural")
            if (!intersection (ht_right, last_ht_left).is_empty ())
              padding += (intersection (ht_right, last_ht_left).length ()
                            ? 0.3    /* edges overlap */
                            : 0.15); /* just touching at the corners */

          mol.add_at_edge (X_AXIS, LEFT, column, padding);

          last_ht_left = ht_right + 3; /* shift up (change to left side) */
          last_glyph_name = glyph_name_scm;
        }
    }

  mol.align_to (X_AXIS, LEFT);

  return mol.smobbed_copy ();
}

ADD_INTERFACE (Key_signature_interface,
               R"(
A group of accidentals, to be printed as signature sign.
               )",

               /* properties */
               R"(
alteration-alist
c0-position
alteration-glyph-name-alist
flat-positions
sharp-positions
padding
padding-pairs
non-default
               )");
