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

#include "gregorian-ligature-engraver.hh"

#include "gregorian-ligature.hh"
#include "international.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "warn.hh"

/* assign_event_once */
#include "translator.icc"

using std::vector;

/*
 * This abstract class is the common superclass for all ligature
 * engravers for Gregorian chant notation.  It cares for the musical
 * handling of the neumes, such as checking for valid combinations of
 * neumes and providing context information.  Notational aspects such
 * as the glyphs to use or calculating the total width of a ligature,
 * are left to the concrete subclass.  Currently, there is only a
 * single subclass, Vaticana_ligature_engraver.  Other ligature
 * engravers for Gregorian chant will be added in the future, such as
 * Medicaea_ligature_engraver or Hufnagel_ligature_engraver.
 */
Gregorian_ligature_engraver::Gregorian_ligature_engraver (Context *c)
  : Coherent_ligature_engraver (c)
{
  pes_or_flexa_req_ = 0;
}

void
Gregorian_ligature_engraver::listen_pes_or_flexa (Stream_event *ev)
{
  assign_event_once (pes_or_flexa_req_, ev);
}

void
fix_prefix (char const *name, int mask, int *current_set, int min_set,
            int max_set, Grob *primitive)
{
  bool current = *current_set & mask;
  bool min = min_set & mask;
  bool max = max_set & mask;
  if (max < min)
    {
      programming_error ("min_set > max_set");
      return;
    }
  if (min && !current)
    {
      primitive->warning (_f ("\\%s ignored", name));
      *current_set &= ~mask;
    }
  if (!max && current)
    {
      primitive->warning (_f ("implied \\%s added", name));
      *current_set |= mask;
    }
}

void
fix_prefix_set (int *current_set, int min_set, int max_set, Grob *primitive)
{
  fix_prefix ("virga", VIRGA, current_set, min_set, max_set, primitive);
  fix_prefix ("stropha", STROPHA, current_set, min_set, max_set, primitive);
  fix_prefix ("inclinatum", INCLINATUM, current_set, min_set, max_set,
              primitive);
  fix_prefix ("auctum", AUCTUM, current_set, min_set, max_set, primitive);
  fix_prefix ("descendens", DESCENDENS, current_set, min_set, max_set,
              primitive);
  fix_prefix ("ascendens", ASCENDENS, current_set, min_set, max_set, primitive);
  fix_prefix ("oriscus", ORISCUS, current_set, min_set, max_set, primitive);
  fix_prefix ("quilisma", QUILISMA, current_set, min_set, max_set, primitive);
  fix_prefix ("deminutum", DEMINUTUM, current_set, min_set, max_set, primitive);
  fix_prefix ("cavum", CAVUM, current_set, min_set, max_set, primitive);
  fix_prefix ("linea", LINEA, current_set, min_set, max_set, primitive);
  fix_prefix ("pes_or_flexa", LINEA, current_set, min_set, max_set, primitive);
}

void
check_and_fix_all_prefixes (vector<Item *> const &primitives)
{
  /* Check for invalid head modifier combinations */
  for (const auto &primitive : primitives)
    {
      /* compute head prefix set by inspecting primitive grob properties */
      int prefix_set
        = (VIRGA * from_scm<bool> (get_property (primitive, "virga")))
          | (STROPHA * from_scm<bool> (get_property (primitive, "stropha")))
          | (INCLINATUM
             * from_scm<bool> (get_property (primitive, "inclinatum")))
          | (AUCTUM * from_scm<bool> (get_property (primitive, "auctum")))
          | (DESCENDENS
             * from_scm<bool> (get_property (primitive, "descendens")))
          | (ASCENDENS * from_scm<bool> (get_property (primitive, "ascendens")))
          | (ORISCUS * from_scm<bool> (get_property (primitive, "oriscus")))
          | (QUILISMA * from_scm<bool> (get_property (primitive, "quilisma")))
          | (DEMINUTUM * from_scm<bool> (get_property (primitive, "deminutum")))
          | (CAVUM * from_scm<bool> (get_property (primitive, "cavum")))
          | (LINEA * from_scm<bool> (get_property (primitive, "linea")))
          | (PES_OR_FLEXA
             * from_scm<bool> (get_property (primitive, "pes-or-flexa")));

      /* check: ascendens and descendens exclude each other; same with
         auctum and deminutum */
      if (prefix_set & DESCENDENS)
        {
          fix_prefix_set (&prefix_set, prefix_set & ~ASCENDENS,
                          prefix_set & ~ASCENDENS, primitive);
        }
      if (prefix_set & AUCTUM)
        {
          fix_prefix_set (&prefix_set, prefix_set & ~DEMINUTUM,
                          prefix_set & ~DEMINUTUM, primitive);
        }

      /* check: virga, quilisma and oriscus cannot be combined with any
         other prefix, but may be part of a pes or flexa */
      if (prefix_set & VIRGA)
        {
          fix_prefix_set (&prefix_set, VIRGA, VIRGA | PES_OR_FLEXA, primitive);
        }
      if (prefix_set & QUILISMA)
        {
          fix_prefix_set (&prefix_set, QUILISMA, QUILISMA | PES_OR_FLEXA,
                          primitive);
        }
      if (prefix_set & ORISCUS)
        {
          fix_prefix_set (&prefix_set, ORISCUS, ORISCUS | PES_OR_FLEXA,
                          primitive);
        }

      /* check: auctum is the only valid optional prefix for stropha */
      if (prefix_set & STROPHA)
        {
          fix_prefix_set (&prefix_set, STROPHA, STROPHA | AUCTUM, primitive);
        }

      /* check: inclinatum may be prefixed with auctum or deminutum only */
      if (prefix_set & INCLINATUM)
        {
          fix_prefix_set (&prefix_set, INCLINATUM,
                          INCLINATUM | AUCTUM | DEMINUTUM, primitive);
        }
      /* check: semivocalis (deminutum but not inclinatum) must occur in
         combination with and only with pes or flexa */
      else if (prefix_set & DEMINUTUM)
        {
          fix_prefix_set (&prefix_set, DEMINUTUM | PES_OR_FLEXA,
                          DEMINUTUM | PES_OR_FLEXA, primitive);
        }

      /* check: cavum and linea (either or both) may be applied only
         upon core punctum */
      if (prefix_set & (CAVUM | LINEA))
        {
          fix_prefix_set (&prefix_set, 0, CAVUM | LINEA, primitive);
        }

      /* all other combinations should be valid (unless I made a
         mistake) */

      set_property (primitive, "prefix-set", to_scm (prefix_set));
    }
}

/*
 * Marks those heads that participate in a pes or flexa.
 */
void
provide_context_info (vector<Item *> const &primitives)
{
  Grob *prev_primitive = 0;
  int prev_prefix_set = 0;
  int prev_context_info = 0;
  int prev_pitch = 0;
  for (vsize i = 0; i < primitives.size (); i++)
    {
      Grob *primitive = primitives[i];
      Stream_event *event_cause = primitive->event_cause ();
      int context_info = 0;
      int pitch = unsmob<Pitch> (get_property (event_cause, "pitch"))->steps ();
      int prefix_set = from_scm<int> (get_property (primitive, "prefix-set"));

      if (prefix_set & PES_OR_FLEXA)
        {
          if (!i) // ligature may not start with 2nd head of pes or flexa
            primitive->warning (
              _ ("cannot apply `\\~' on first head of ligature"));
          else if (pitch > prev_pitch) // pes
            {
              prev_context_info |= PES_LOWER;
              context_info |= PES_UPPER;
            }
          else if (pitch < prev_pitch) // flexa
            {
              prev_context_info |= FLEXA_LEFT;
              context_info |= FLEXA_RIGHT;
            }
          else // (pitch == prev_pitch)
            primitive->warning (
              _ ("cannot apply `\\~' on heads with identical pitch"));
        }
      if (prev_prefix_set & DEMINUTUM)
        context_info |= AFTER_DEMINUTUM;

      if (prev_primitive)
        set_property (prev_primitive, "context-info",
                      to_scm (prev_context_info));
      prev_primitive = primitive;
      prev_prefix_set = prefix_set;
      prev_context_info = context_info;
      prev_pitch = pitch;
    }
  if (prev_primitive)
    set_property (prev_primitive, "context-info", to_scm (prev_context_info));
}

void
Gregorian_ligature_engraver::build_ligature (Spanner *ligature,
                                             vector<Item *> const &primitives)
{
  // apply style-independent checking and transformation
  check_and_fix_all_prefixes (primitives);
  provide_context_info (primitives);

  // apply style-specific transformation (including line-up); to be
  // implemented by subclass
  transform_heads (ligature, primitives);
}

void
Gregorian_ligature_engraver::stop_translation_timestep ()
{
  Ligature_engraver::stop_translation_timestep ();
  pes_or_flexa_req_ = 0;
}

// no ADD_ACKNOWLEDGER / ADD_ACKNOWLEDGER / ADD_TRANSLATOR macro calls
// since this class is abstract
