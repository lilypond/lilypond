/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Juergen Reuter <reuter@ipd.uka.de>,
  Pal Benko <benkop@freestart.hu>

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
#include "mensural-ligature.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

/*
 * TODO: accidentals are aligned with the first note;
 * they must appear ahead.
 *
 * TODO: prohibit ligatures having notes differing only in accidentals
 * (like \[ a\breve g as \])
 *
 * TODO: do something with multiple voices within a ligature.  See
 * for example:
 * Ockeghem: Missa Ecce ancilla domini, bassus part, end of Christe.
 *
 * TODO: enhance robustness: in case of an invalid ligature (e.g. the
 * input specifies a ligature that contains a minima), automatically
 * break the ligature into smaller, valid pieces.  Such a piece may be
 * a single note.
 */

class Mensural_ligature_engraver : public Coherent_ligature_engraver
{

protected:
  Spanner *create_ligature_spanner () override;
  void build_ligature (Spanner *ligature,
                       vector<Item *> const &primitives) override;

public:
  TRANSLATOR_DECLARATIONS (Mensural_ligature_engraver);

private:
  void transform_heads (vector<Item *> const &primitives);
  void propagate_properties (Spanner *ligature,
                             vector<Item *> const &primitives,
                             Real &min_length);
  void fold_up_primitives (vector<Item *> const &primitives, Real &min_length);
};

Mensural_ligature_engraver::Mensural_ligature_engraver (Context *c)
  : Coherent_ligature_engraver (c)
{
  brew_ligature_primitive_proc
    = Mensural_ligature::brew_ligature_primitive_proc;
}

Spanner *
Mensural_ligature_engraver::create_ligature_spanner ()
{
  return make_spanner ("MensuralLigature", SCM_EOL);
}

void
Mensural_ligature_engraver::transform_heads (vector<Item *> const &primitives)
{
  if (primitives.size () < 2)
    {
      warning (_ ("ligature with less than 2 heads -> skipping"));
      return;
    }
  int prev_pitch = 0;
  bool at_beginning = true;

  // needed so that we can check whether
  // the previous note can be turned into a flexa
  bool prev_brevis_shape = false;

  bool prev_semibrevis = false;
  Item *prev_primitive = NULL;

  for (vsize i = 0, s = primitives.size (); i < s; i++)
    {
      auto *const primitive = primitives[i];
      int duration_log = Rhythmic_head::duration_log (primitive);

      Stream_event *nr = primitive->event_cause ();

      /*
        ugh. why not simply check for pitch?
      */
      if (!nr->in_event_class ("note-event"))
        {
          nr->warning (
            _ ("cannot determine pitch of ligature primitive -> skipping"));
          at_beginning = true;
          continue;
        }

      int pitch = unsmob<Pitch> (get_property (nr, "pitch"))->steps ();
      int prim = 0;

      if (at_beginning)
        {
          if (i == s - 1)
            {
              // we can get here after invalid input
              nr->warning (_ ("single note ligature - skipping"));
              break;
            }
          prev_semibrevis = prev_brevis_shape = false;
          prev_primitive = NULL;
        }
      else
        {
          if (pitch == prev_pitch)
            {
              nr->warning (_ ("prime interval within ligature -> skipping"));
              at_beginning = true;
              prim = MLP_NONE;
              continue;
            }
        }

      if (duration_log < -3 // is this possible at all???
          || duration_log > 0)
        {
          nr->warning (
            _ ("mensural ligature: duration none of Mx, L, B, S -> skipping"));
          prim = MLP_NONE;
          at_beginning = true;
          continue;
        }

      bool general_case = true;
      bool make_flexa = false;
      bool allow_flexa = true;

      // first check special cases
      // 1. beginning
      if (at_beginning)
        {
          // a. semibreves
          if (duration_log == 0)
            {
              prim = MLP_UP | MLP_BREVIS;
              general_case = false;
            }
          // b. descendens longa or brevis
          else if (i < s - 1
                   && (unsmob<Pitch> (
                         get_property (primitives[i + 1]->event_cause (),
                                       "pitch"))
                         ->steps ()
                       < pitch)
                   && duration_log > -3)
            {
              int left_stem = duration_log == -1 ? MLP_DOWN : 0;
              prim = left_stem | MLP_BREVIS;
              general_case = false;
            }
        }
      // 2. initial semibrevis must be followed by another one
      else if (prev_semibrevis)
        {
          prev_semibrevis = false;
          if (duration_log == 0)
            {
              prim = MLP_BREVIS;
              general_case = false;
            }
          else
            {
              nr->warning (
                _ ("semibrevis must be followed by another one -> skipping"));
              prim = MLP_NONE;
              at_beginning = true;
              continue;
            }
        }
      // 3. semibreves are otherwise not allowed
      else if (duration_log == 0)
        {
          nr->warning (
            _ ("semibreves can only appear at the beginning of a ligature,\n"
               "and there may be only zero or two of them"));
          prim = MLP_NONE;
          at_beginning = true;
          continue;
        }
      // 4. end, descendens
      else if (i == s - 1 && pitch < prev_pitch)
        {
          // brevis; previous note must be turned into flexa
          if (duration_log == -1)
            {
              if (prev_brevis_shape)
                {
                  make_flexa = true;
                  general_case = false;
                }
              else
                {
                  nr->warning (_ ("invalid ligatura ending:\n"
                                  "when the last note is a descending brevis,\n"
                                  "the penultimate note must be another one,\n"
                                  "or the ligatura must be LB or SSB"));
                  prim = MLP_NONE;
                  break;
                }
            }
          // longa
          else if (duration_log == -2)
            {
              prim = MLP_BREVIS;
              general_case = allow_flexa = false;
            }
          // else maxima; fall through to regular case below
        }

      if (allow_flexa
          && from_scm<bool> (get_property (primitive, "ligature-flexa")))
        {
          /*
            flexa requested, check whether allowed:
            - there should be a previous note
            - both of the notes must be of brevis shape
              (i.e. can't be maxima or flexa;
              longa is forbidden as well - it's nonexistent anyway)
            - no compulsory flexa for the next note,
              i.e. it's not an ultimate descending breve
          */
          make_flexa = !at_beginning && prev_brevis_shape && duration_log > -2;
          if (make_flexa && i == s - 2)
            {
              /*
                check last condition: look ahead to next note
              */
              auto *const next_primitive = primitives[i + 1];
              if (Rhythmic_head::duration_log (next_primitive) == -1)
                {
                  /*
                    breve: check whether descending
                  */
                  int const next_pitch
                    = unsmob<Pitch> (
                        get_property (next_primitive->event_cause (), "pitch"))
                        ->steps ();
                  if (next_pitch < pitch)
                    /*
                      sorry, forbidden
                    */
                    make_flexa = false;
                }
            }
        }

      if (general_case)
        {
          static int const shape[3] = {MLP_MAXIMA, MLP_LONGA, MLP_BREVIS};

          prim = shape[duration_log + 3];
        }

      if (make_flexa)
        {
          /*
            turn the note with the previous one into a flexa
          */
          set_property (prev_primitive, "primitive",
                        to_scm (MLP_FLEXA_BEGIN
                                | (from_scm<int> (
                                     get_property (prev_primitive, "primitive"))
                                   & MLP_STEM)));
          set_property (prev_primitive, "flexa-interval",
                        to_scm (pitch - prev_pitch));
          prim = MLP_FLEXA_END;
          set_property (primitive, "flexa-interval",
                        to_scm (pitch - prev_pitch));
        }

      // join_primitives replacement
      if (!(at_beginning || make_flexa))
        set_property (prev_primitive, "add-join", SCM_BOOL_T);

      at_beginning = false;
      prev_primitive = primitive;
      prev_pitch = pitch;
      set_property (primitive, "primitive", to_scm (prim));
      prev_brevis_shape = (prim & MLP_BREVIS) != 0;
      prev_semibrevis = (prim & MLP_UP) != 0;
    }
}

/*
 * A MensuralLigature grob consists of a bunch of NoteHead grobs that
 * are glued together.  It (a) does not make sense to change
 * properties like thickness or flexa-width from one head to the next
 * within a ligature (this would totally screw up alignment), and (b)
 * some of these properties (like flexa-width) are specific to
 * e.g. the MensuralLigature (as in contrast to e.g. LigatureBracket),
 * and therefore should not be handled in the NoteHead code (which is
 * also used by LigatureBracket).  Therefore, we let the user control
 * these properties via the concrete Ligature grob (like
 * MensuralLigature) and then copy these properties as necessary to
 * each of the NoteHead grobs.  This is what
 * propagate_properties () does.
 */
void
Mensural_ligature_engraver::propagate_properties (
  Spanner *ligature, vector<Item *> const &primitives, Real &min_length)
{
  Real thickness = from_scm<double> (get_property (ligature, "thickness"), 1.3);
  thickness
    *= ligature->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));

  Real head_width = Font_interface::get_default_font (ligature)
                      ->find_by_name ("noteheads.sM1mensural")
                      .extent (X_AXIS)
                      .length ();
  Real maxima_head_width = Font_interface::get_default_font (ligature)
                             ->find_by_name ("noteheads.sM3ligmensural")
                             .extent (X_AXIS)
                             .length ();

  min_length = 0.0;
  Item *prev_primitive = NULL;
  for (const auto &primitive : primitives)
    {
      int output = from_scm<int> (get_property (primitive, "primitive"));
      set_property (primitive, "thickness", to_scm (thickness));

      switch (output & MLP_ANY)
        {
        case MLP_BREVIS:
        case MLP_LONGA:
          min_length += head_width;
          set_property (primitive, "head-width", to_scm (head_width));
          break;
        case MLP_MAXIMA:
          min_length += maxima_head_width;
          set_property (primitive, "head-width", to_scm (maxima_head_width));
          break;
        case MLP_FLEXA_BEGIN:
          /*
            the next note (should be MLP_FLEXA_END) will handle this one
          */
          break;
        case MLP_FLEXA_END:
          {
            SCM flexa_scm = get_property (primitive, "flexa-width");
            Real const flexa_width = from_scm<double> (flexa_scm, 2.0);
            min_length += flexa_width + thickness;
            SCM head_width = to_scm (0.5 * (flexa_width + thickness));
            set_property (primitive, "head-width", head_width);
            set_property (prev_primitive, "head-width", head_width);
            set_property (prev_primitive, "flexa-width", flexa_scm);
          }
          break;
        default:
          programming_error (_ ("unexpected case fall-through"));
          break;
        }

      prev_primitive = primitive;
    }
}

void
Mensural_ligature_engraver::fold_up_primitives (
  vector<Item *> const &primitives, Real &min_length)
{
  Item *first = 0;
  Real distance = 0.0;
  Real staff_space = 0.0;
  Real thickness = 0.0;

  for (vsize i = 0; i < primitives.size (); i++)
    {
      auto *const current = primitives[i];
      if (i == 0)
        {
          first = current;
          staff_space = Staff_symbol_referencer::staff_space (first);
          thickness = from_scm<double> (get_property (current, "thickness"));
        }

      move_related_items_to_column (current, first->get_column (), distance);

      Real head_width = from_scm<double> (get_property (current, "head-width"));
      distance += head_width - thickness;

      if (size_t const dot_count = Rhythmic_head::dot_count (current))
        /*
          Move dots above/behind the ligature.
          dots should also avoid staff lines.
        */
        {
          Grob *dot_gr = Rhythmic_head::get_dots (current);

          bool const on_line = Staff_symbol_referencer::on_line (
            current, from_scm (get_property (current, "staff-position"), 0));
          Real vert_shift = on_line ? staff_space * 0.5 : 0.0;
          bool const flexa_begin
            = from_scm<int> (get_property (current, "primitive"))
              & MLP_FLEXA_BEGIN;

          if (i + 1 < primitives.size ())
            /*
              dot in the midst => avoid next note;
              what to avoid and where depends on
              being on a line or between lines
            */
            {
              int const delta
                = from_scm<int> (get_property (current, "delta-position"));
              if (flexa_begin)
                vert_shift += delta < 0 ? staff_space
                                        : (on_line ? -2.0 : -1.0) * staff_space;
              else if (on_line)
                {
                  if (0 < delta && delta < 3)
                    vert_shift -= staff_space;
                }
              else if (delta == 1 || delta == -1)
                vert_shift -= delta * staff_space;
            }
          else
            min_length += head_width * static_cast<Real> (dot_count);

          dot_gr->translate_axis (vert_shift, Y_AXIS);

          /*
            move all dots behind head

            This is ugly and should probably be handled by configuring
            the DotColumn appropriately.  Note that these dots will
            be disconnected from their dot column.  See
            move_related_items_to_column.

            This also means the padding isn't configurable as DotColumn.padding is.
          */
          const Stencil *stil
            = unsmob<const Stencil> (get_property (dot_gr, "dot-stencil"));
          Real dot_width = stil ? stil->extent (X_AXIS).length () : 0.0;
          dot_gr->translate_axis ((flexa_begin ? staff_space * 0.6 : head_width)
                                    - 2.0 * thickness + dot_width,
                                  X_AXIS);
        }
    }
}

void
Mensural_ligature_engraver::build_ligature (Spanner *ligature,
                                            vector<Item *> const &primitives)
{
  /*
    the X extent of the actual graphics representing the ligature;
    less space than that means collision
  */
  Real min_length;

  transform_heads (primitives);
  propagate_properties (ligature, primitives, min_length);
  fold_up_primitives (primitives, min_length);

  if (from_scm<double> (get_property (ligature, "minimum-length"), 0.0)
      < min_length)
    set_property (ligature, "minimum-length", to_scm (min_length));
}

void
Mensural_ligature_engraver::boot ()
{
  ADD_DELEGATE_LISTENER (ligature);
  ADD_ACKNOWLEDGER (rest);
  ADD_ACKNOWLEDGER (ligature_head);
}

ADD_TRANSLATOR (Mensural_ligature_engraver,
                /* doc */
                R"(
Handle @code{Mensural_ligature_events} by glueing special ligature heads
together.
                )",

                /* create */
                R"(
MensuralLigature
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
