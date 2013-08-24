/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "output-def.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "stencil.hh"
#include "skyline-pair.hh"

Stencil
parenthesize (Grob *me, Stencil m)
{
  Font_metric *font
    = Font_interface::get_default_font (me);
  Stencil open
    = font->find_by_name ("accidentals.leftparen");
  Stencil close
    = font->find_by_name ("accidentals.rightparen");

  m.add_at_edge (X_AXIS, LEFT, Stencil (open), 0);
  m.add_at_edge (X_AXIS, RIGHT, Stencil (close), 0);

  return m;
}

/* If this gets called before line breaking, we will return a non-trivial
   extent even if we belong to a tie and won't actually get printed. */
static SCM
get_extent (Grob *me, Axis a)
{
  Stencil *s = unsmob_stencil (Accidental_interface::get_stencil (me));

  if (s)
    return ly_interval2scm (s->extent (a));
  return ly_interval2scm (Interval ());
}

MAKE_SCHEME_CALLBACK (Accidental_interface, height, 1);
SCM
Accidental_interface::height (SCM smob)
{
  return get_extent (unsmob_grob (smob), Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Accidental_interface, width, 1);
SCM
Accidental_interface::width (SCM smob)
{
  return get_extent (unsmob_grob (smob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Accidental_interface, horizontal_skylines, 1);
SCM
Accidental_interface::horizontal_skylines (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (!me->is_live ())
    return Skyline_pair ().smobbed_copy ();

  /*
   * Using the print function may trigger a suicide
   * before line breaking. It is therefore `unpure' (c).
   * We use the more basic get_stencil.
   */
  Stencil *my_stencil = unsmob_stencil (get_stencil (me));
  if (!my_stencil)
    return Skyline_pair ().smobbed_copy ();

  Skyline_pair *sky =
    Skyline_pair::unsmob
      (Stencil::skylines_from_stencil
        (my_stencil->smobbed_copy (), 0.0, Y_AXIS));

  SCM alist = me->get_property ("glyph-name-alist");
  SCM alt = me->get_property ("alteration");
  string glyph_name = robust_scm2string (ly_assoc_get (alt, alist, SCM_BOOL_F),
                                                       "");
  if (glyph_name == "accidentals.flat"
      || glyph_name == "accidentals.flatflat")
    {
      // a bit more padding for the right of the stem
      // we raise the stem horizontally to a bit less than the average
      // horizontal "height" of the entire glyph. This will bring flats
      // closer to doubleflats, which looks better (MS opinion).
      // this should work for all fonts where the flat is not
      // completely bizarre
      Real left = my_stencil->extent (X_AXIS)[LEFT];
      Real right = my_stencil->extent (X_AXIS)[RIGHT] * 0.375;
      Real down = my_stencil->extent (Y_AXIS)[DOWN];
      Real up = my_stencil->extent (Y_AXIS)[UP];
      vector<Box> boxes;
      boxes.push_back (Box (Interval (left, right), Interval (down, up)));
      Skyline merge_with_me (boxes, Y_AXIS, RIGHT);
      (*sky)[RIGHT].merge (merge_with_me);
    }
  return sky->smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Accidental_interface, pure_height, 3);
SCM
Accidental_interface::pure_height (SCM smob, SCM start_scm, SCM)
{
  Item *me = dynamic_cast<Item *> (unsmob_grob (smob));
  int start = scm_to_int (start_scm);
  int rank = me->get_column ()->get_rank ();

  if (to_boolean (me->get_property ("forced"))
      || !unsmob_grob (me->get_object ("tie"))
      || (rank == start + 1 && /* we are at the start of a line */
          !to_boolean (me->get_property ("hide-tied-accidental-after-break"))))
    {
      Stencil *s = unsmob_stencil (get_stencil (me));
      if (s)
        return ly_interval2scm (s->extent (Y_AXIS));
    }

  return ly_interval2scm (Interval ());
}

MAKE_SCHEME_CALLBACK (Accidental_interface, print, 1);
SCM
Accidental_interface::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *tie = unsmob_grob (me->get_object ("tie"));

  if (tie
      && (to_boolean (me->get_property ("hide-tied-accidental-after-break"))
          || (!tie->original () && !to_boolean (me->get_property ("forced")))))
    {
      me->suicide ();
      return SCM_EOL;
    }

  return get_stencil (me);
}

SCM
Accidental_interface::get_stencil (Grob *me)
{
  Font_metric *fm = Font_interface::get_default_font (me);

  SCM alist = me->get_property ("glyph-name-alist");
  SCM alt = me->get_property ("alteration");
  SCM glyph_name = ly_assoc_get (alt, alist, SCM_BOOL_F);

  if (!scm_is_string (glyph_name))
    {
      me->warning (_f ("Could not find glyph-name for alteration %s",
                       ly_scm_write_string (alt).c_str ()));
      return SCM_EOL;
    }

  Stencil mol (fm->find_by_name (ly_scm2string (glyph_name)));
  if (to_boolean (me->get_property ("restore-first")))
    {
      /*
        this isn't correct for ancient accidentals, but they don't
        use double flats/sharps anyway.
        */
      Stencil acc (fm->find_by_name ("accidentals.natural"));

      if (acc.is_empty ())
        me->warning (_ ("natural alteration glyph not found"));
      else
        mol.add_at_edge (X_AXIS, LEFT, acc, 0.1);
    }

  if (to_boolean (me->get_property ("parenthesized")))
    mol = parenthesize (me, mol);

  return mol.smobbed_copy ();
}

ADD_INTERFACE (Accidental_interface,
               "A single accidental.",

               /* properties */
               "alteration "
               "avoid-slur "
               "forced "
               "glyph-name-alist "
               "glyph-name "
               "hide-tied-accidental-after-break "
               "parenthesized "
               "restore-first "
               "tie "
              );
