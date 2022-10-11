/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "lazy-skyline-pair.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "skyline-pair.hh"
#include "stencil.hh"
#include "system.hh"

using std::string;
using std::vector;

Stencil
parenthesize (Grob *me, Stencil m)
{
  Font_metric *font = Font_interface::get_default_font (me);
  Stencil open = font->find_by_name ("accidentals.leftparen");
  Stencil close = font->find_by_name ("accidentals.rightparen");

  m.add_at_edge (X_AXIS, LEFT, open, 0);
  m.add_at_edge (X_AXIS, RIGHT, close, 0);

  return m;
}

MAKE_SCHEME_CALLBACK (Accidental_interface, horizontal_skylines,
                      "ly:accidental-interface::horizontal-skylines", 1);
SCM
Accidental_interface::horizontal_skylines (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  if (!me->is_live ())
    return to_scm (Skyline_pair ());

  auto *my_stencil = unsmob<const Stencil> (get_property (me, "stencil"));
  if (!my_stencil)
    return to_scm (Skyline_pair ());

  Skyline_pair sky (skylines_from_stencil (
    my_stencil->smobbed_copy (), get_property (me, "rotation"), Y_AXIS));

  SCM parenthesized = get_property (me, "parenthesized");

  string glyph_name = ly_scm2string (get_property (me, "glyph-name"));

  if ((glyph_name == "accidentals.flat" || glyph_name == "accidentals.flatflat")
      && !from_scm<bool> (parenthesized))
    {
      // a bit more padding for the right of the stem
      // we raise the stem horizontally to a bit less than the average
      // horizontal "height" of the entire glyph. This will bring flats
      // closer to doubleflats, which looks better (MS opinion).
      // this should work for all fonts where the flat is not
      // completely bizarre
      Real left = my_stencil->extent (X_AXIS)[LEFT];
      Real right = my_stencil->extent (X_AXIS)[RIGHT] * 0.375;
      vector<Box> boxes;
      boxes.push_back (
        Box (Interval (left, right), my_stencil->extent (Y_AXIS)));
      Skyline merge_with_me (boxes, Y_AXIS, RIGHT);
      sky[RIGHT].merge (merge_with_me);
    }
  return to_scm (sky);
}

MAKE_SCHEME_CALLBACK (Accidental_interface, height,
                      "ly:accidental-interface::height", 1);
SCM
Accidental_interface::height (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *tie = unsmob<Grob> (get_object (me, "tie"));

  if (tie && !from_scm<bool> (get_property (me, "forced"))
      && from_scm<bool> (get_property (me, "hide-tied-accidental-after-break")))
    return to_scm (Interval ());

  return Grob::stencil_height (smob);
}

MAKE_SCHEME_CALLBACK (Accidental_interface, remove_tied,
                      "ly:accidental-interface::remove-tied", 1);
SCM
Accidental_interface::remove_tied (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *tie = unsmob<Grob> (get_object (me, "tie"));

  if (tie && !from_scm<bool> (get_property (me, "forced"))
      && (from_scm<bool> (get_property (me, "hide-tied-accidental-after-break"))
          || !tie->original ()))
    me->suicide ();

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Accidental_interface, print,
                      "ly:accidental-interface::print", 1);
SCM
Accidental_interface::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Font_metric *fm = Font_interface::get_default_font (me);
  std::string glyph_name = ly_scm2string (get_property (me, "glyph-name"));
  Stencil st = fm->find_by_name (glyph_name);
  if (st.is_empty ())
    me->warning (_f ("cannot find glyph %s", glyph_name));

  if (from_scm<bool> (get_property (me, "restore-first")))
    {
      /*
        this isn't correct for ancient accidentals, but they don't
        use double flats/sharps anyway.
        */
      Stencil acc (fm->find_by_name ("accidentals.natural"));

      if (acc.is_empty ())
        me->warning (_ ("natural alteration glyph not found"));
      else
        st.add_at_edge (X_AXIS, LEFT, acc, 0.1);
    }

  if (from_scm<bool> (get_property (me, "parenthesized")))
    st = parenthesize (me, st);

  return st.smobbed_copy ();
}

ADD_INTERFACE (Accidental_interface,
               R"(
A single accidental.
               )",

               /* properties */
               R"(
alteration
alteration-glyph-name-alist
avoid-slur
glyph-name
forced
hide-tied-accidental-after-break
restore-first
tie
               )");
