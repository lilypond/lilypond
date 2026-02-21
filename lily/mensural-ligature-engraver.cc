/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2026 Juergen Reuter <reuter@ipd.uka.de>,
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

/*
 * TODO: accidentals are aligned with the first note;
 * they must appear ahead.
 *
 * TODO: prohibit ligatures having notes differing only in accidentals
 * (like \[ a\breve g as \])
 *
 * TODO: do something with multiple voices within a ligature.  See
 * for example: Trent 91, f178v or Aosta 15 f135r
 *
 * TODO: enhance robustness: in case of an invalid ligature (e.g. the
 * input specifies a ligature that contains a minima), automatically
 * break the ligature into smaller, valid pieces.  Such a piece may be
 * a single note.
 */

class Mensural_ligature_engraver final : public Coherent_ligature_engraver
{

protected:
  Spanner *create_ligature_spanner () override;
  void build_ligature (Spanner *ligature,
                       std::vector<Item *> const &primitives) override;

public:
  TRANSLATOR_DECLARATIONS (Mensural_ligature_engraver);

private:
  void transform_heads (std::vector<Item *> const &primitives);
  void propagate_properties (Spanner *ligature,
                             std::vector<Item *> const &primitives,
                             Real &min_length);
  void fold_up_primitives (std::vector<Item *> const &primitives,
                           Real &min_length);
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
Mensural_ligature_engraver::transform_heads (
  std::vector<Item *> const &primitives)
{
  if (primitives.empty ())
    {
      warning (_ ("empty ligature"));
    }
  int prev_pitch = 0, prev_prim = 0;
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
            _ ("cannot determine pitch of ligature primitive; skipping"));
          at_beginning = true;
          continue;
        }

      int pitch = unsmob<Pitch> (get_property (nr, "pitch"))->steps ();
      int prim = 0;
      bool const is_last = i == s - 1;
      auto *const next_primitive = is_last ? nullptr : primitives[i + 1];
      int const next_pitch = is_last
        ? 0
        : unsmob<Pitch> (get_property (next_primitive->event_cause (),
                                       "pitch"))
            ->steps ();
      int const next_dur
        = is_last ? 0 : Rhythmic_head::duration_log (next_primitive);

      if (!at_beginning && pitch == prev_pitch)
        nr->warning (_ ("unison within ligature"));

      bool general_case = true;
      bool make_flexa = false;
      bool const is_brevis = duration_log == -1;
      bool const is_longa = duration_log == -2;

      if (duration_log < -3 // is this possible at all???
          || duration_log > 0)
        {
          nr->warning (
            _ ("mensural ligature:"
               " duration none of maxima, longa, breve, or semibreve"));
          prim = MLP_INVALID;
        }
      else if (at_beginning && is_last)
        nr->warning (_ ("single note ligature"));
      // check descending cases
      // 1. at start
      else if (at_beginning && next_pitch < pitch && (is_brevis || is_longa))
        {
          int const left_stem = is_brevis ? MLP_DOWN : 0;
          prim = left_stem | MLP_BREVIS;
        }
      // 2. at end
      else if (is_last && pitch < prev_pitch)
        {
          // brevis; should form a flexa with the previous note
          if (is_brevis)
            {
              if (prev_brevis_shape)
                {
                  make_flexa = true;
                  general_case = false;
                }
              else
                /*
                  flexa impossible;
                  instead of refusal, add right stem to the previous note
                */
                prim = MLP_BREVIS | MLP_DOWN;
            }
          // longa
          else if (is_longa && !prev_semibrevis)
            prim = MLP_BREVIS;
          // else fall through to regular case below
        }

      if (from_scm<bool> (get_property (primitive, "ligature-pes")))
        {
          if (is_last && duration_log < -1 && prev_pitch + 1 < pitch)
            {
              prim = (is_longa ? MLP_BREVIS : MLP_MAXIMA) | MLP_PES;
            }
          else
            nr->warning (_ ("only a final longa higher at least by a third "
                            "than the previous note\n"
                            "can be drawn pes-like"));
        }

      if (general_case && !(prim & MLP_ANY))
        {
          static int const shape[4] = {MLP_MAXIMA, MLP_BREVIS | MLP_JOIN_DOWN,
                                       MLP_BREVIS, MLP_BREVIS};

          prim = shape[duration_log + 3];
          if (prev_semibrevis)
            {
              if (is_brevis)
                {
                  nr->warning
                    (_ ("single semibreve must not be followed by a breve"));
                  /*
                    nevertheless show breve by a left down stem
                  */
                  prim |= MLP_DOWN;
                }
            }
          else if (duration_log == 0)
            /*
              semibreve pairs are denoted by an upward tail on the left
              (theoretical sources require them at the beginning, but
              an upward tail is not used for anything else in the middle,
              and a few codices use it to denote semibreves,
              see e.g. Fayrfax's Aeternae laudis lilium in the
              Lambeth Choirbook: fol. 58v, start of the fourth line)
            */
            prim |= MLP_UP;
        }

      /*
        check whether this and the previous note
        may/must/can't be turned into flexa:
        - there should be a previous note
        - both of the notes must be of brevis shape
          (i.e. can't be maxima or flexa)
        - there mustn't be a stem between the two notes
        - if the next note is an ultimate descending breve,
          this note must form a flexa with that, not with the previous one
        - if the next note is a high enough pes-like final longa
          and the previous note is higher than the current one,
          then the three note must form a porrectus (i.e. flexa here)
          to avoid collision of the previous and the next note
      */
      bool flexa_possible = !at_beginning && prev_brevis_shape
                            && !(prim & (MLP_STEM | MLP_MAXIMA | MLP_INVALID));
      bool const flexa_requested
        = from_scm<bool> (get_property (primitive, "ligature-flexa"));
      if (flexa_requested && !flexa_possible && !(prim & MLP_INVALID))
        {
          if (at_beginning)
            nr->warning
              (_ ("tweak ligature-flexa between the two required notes"));
          else if ((prev_prim | prim) & MLP_MAXIMA)
            nr->warning (_ ("maxima cannot form part of a flexa"));
          else
            nr->warning (_ ("flexa cannot have stem in the middle"));
        }

      if (flexa_possible && i == s - 2)
        {
          /*
            penultimate note:
            final note is to be checked for the must/can't conditions
          */

          if (next_dur == -1)
            {
              /*
                breve: check whether descending
              */
              if (next_pitch < pitch)
                {
                  flexa_possible = false;
                  if (flexa_requested)
                    nr->warning
                      (_ ("this note must form a flexa with the next note,\n"
                          "not the previous one"));
                }
            }
          else if (next_dur == -2)
            {
              /*
                longa: check whether ascending high enough
                and pes is requested
              */
              if (next_pitch < prev_pitch + 4 && pitch < prev_pitch
                  && pitch + 1 < next_pitch
                  && from_scm<bool> (get_property (next_primitive,
                                                   "ligature-pes")))
                {
                  make_flexa = true;
                }
            }
        }
      if (!make_flexa)
        make_flexa = flexa_requested && flexa_possible;

      if (make_flexa && !(prim & (MLP_PES | MLP_INVALID)))
        {
          /*
            turn the note with the previous one into a flexa
          */
          set_property (
            prev_primitive, "primitive",
            to_scm (
              MLP_FLEXA_BEGIN
              | (from_scm<int> (get_property (prev_primitive, "primitive"), 0)
                 & MLP_STEM)));
          set_property (prev_primitive, "flexa-interval",
                        to_scm (pitch - prev_pitch));
          prim &= ~MLP_BREVIS;
          prim |= MLP_FLEXA_END;
          set_property (primitive, "flexa-interval",
                        to_scm (pitch - prev_pitch));

          if (is_longa)
            {
              /*
                flexa ending in a longa:
                right stem needed explicitly even at descending end of ligature
              */
              prim |= MLP_JOIN_DOWN;
            }
        }

      if (from_scm<bool> (get_property (primitive, "left-down-stem")))
        {
          if (at_beginning && is_brevis)
            prim |= MLP_DOWN;
          else
            nr->warning
              (_ (" only an initial breve can have downward left stem"));
        }

      if (from_scm<bool> (get_property (primitive, "right-down-stem")))
        {
          if (duration_log < -1)
            prim |= MLP_JOIN_DOWN;
          else
            nr->warning (_ ("only longae and maximae may have right stem"));
        }

      if (from_scm<bool> (get_property (primitive, "right-up-stem")))
        {
          if (duration_log < -1)
            {
              if (i + 2 < s && next_dur > -2)
                nr->warning
                  (_ ("in the middle of the ligature an upward stem\n"
                      "belongs more often to the next note"));

              prim &= ~MLP_JOIN_DOWN;
              prim |= MLP_JOIN_UP;
            }
          else
            nr->warning (_ ("only longae and maximae may have right stem"));
        }

      // join_primitives replacement
      if (!(at_beginning || make_flexa))
        set_property (prev_primitive, "add-join", SCM_BOOL_T);

      at_beginning = false;
      prev_primitive = primitive;
      prev_pitch = pitch;
      prev_prim = prim;
      set_property (primitive, "primitive", to_scm (prim));
      prev_brevis_shape = (prim & (MLP_ANY | MLP_RIGHT_STEM)) == MLP_BREVIS;
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
  Spanner *ligature, std::vector<Item *> const &primitives, Real &min_length)
{
  Real thickness
    = from_scm<double> (get_property (ligature, "thickness"), 1.3);
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
  /*
    start with the width of the first vertical edge,
    then let each note head add its own increment,
    considering that its left edge is taken account of
    (with either this initialization or the right edge of the previous head)
  */
  min_length = thickness;

  Item *prev_primitive = NULL;
  int prev_output;
  for (const auto &primitive : primitives)
    {
      int const output
        = from_scm<int> (get_property (primitive, "primitive"), 0);
      set_property (primitive, "thickness", to_scm (thickness));

      switch (output & MLP_ANY)
        {
        case MLP_INVALID:
        case MLP_BREVIS:
          if (!(output & MLP_PES))
            min_length += head_width - thickness;
          set_property (primitive, "head-width", to_scm (head_width));
          break;
        case MLP_MAXIMA:
          min_length += maxima_head_width - thickness;
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
            min_length += flexa_width;
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

      /*
        join to the previous notehead is handled with the previous note.
        let it know when this note has a left stem,
        as it mustn't be hidden by the join
      */
      if (prev_primitive)
        if (int const stem = output & MLP_STEM)
          set_property (prev_primitive, "primitive",
                        to_scm (prev_output | stem * (MLP_JOIN_UP / MLP_UP)));

      prev_primitive = primitive;
      prev_output = output;
    }
}

void
Mensural_ligature_engraver::fold_up_primitives (
  std::vector<Item *> const &primitives, Real &min_length)
{
  Item *first = 0;
  Real distance = 0.0;
  Real staff_space = 0.0;
  Real thickness = 0.0;

  for (vsize i = 0, pnum = primitives.size (); i < pnum; i++)
    {
      auto *const current = primitives[i];
      if (i == 0)
        {
          first = current;
          staff_space = Staff_symbol_referencer::staff_space (first);
          thickness
            = from_scm<double> (get_property (current, "thickness"), 0.13);
        }

      move_related_items_to_column (current, first->get_column (), distance);

      int const prim = from_scm<int> (get_property (current, "primitive"), 0);
      Real head_width
        = from_scm<double> (get_property (current, "head-width"), 0.0);
      if (!(prim & MLP_PES))
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
          bool const flexa_begin = prim & MLP_FLEXA_BEGIN;

          if (i + 1 < pnum)
            /*
              dot in the midst => avoid next note;
              what to avoid and where depends on
              being on a line or between lines
            */
            {
              int const delta
                = from_scm<int> (get_property (current, "delta-position"), 0);
              if (flexa_begin)
                {
                  if ((0 < delta && delta < 3 + 2 * on_line)
                      || (!on_line && -3 < delta && delta < 0))
                    {
                      vert_shift += delta < 0 ? staff_space : -staff_space;
                    }
                }
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

            This also means the padding isn't configurable
            as DotColumn.padding is.
          */
          const Stencil *stil
            = unsmob<const Stencil> (get_property (dot_gr, "dot-stencil"));
          Real dot_width = stil ? stil->extent (X_AXIS).length () : 0.0;
          dot_gr->translate_axis (
            (flexa_begin
             ? staff_space * 0.6
             : prim & MLP_PES ? 0.0 : head_width - 0.2 * staff_space)
            - 2.0 * thickness + dot_width,
            X_AXIS);
        }
    }
}

void
Mensural_ligature_engraver::build_ligature (
  Spanner *ligature, std::vector<Item *> const &primitives)
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
