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

#include "font-interface.hh"
#include "gregorian-ligature.hh"
#include "international.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "separation-item.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "vaticana-ligature.hh"
#include "warn.hh"
#include "dot-column.hh"
#include "rhythmic-head.hh"
#include "pitch.hh"
#include "translator.icc"

using std::string;
using std::vector;

/*
 * This class implements the notation specific aspects of Vaticana
 * style ligatures for Gregorian chant notation.
 */

/*
 * TODO: Maybe move handling of dots/mora to
 * Gregorian_ligature_engraver?  It's probably common for all types of
 * Gregorian chant notation that have dotted notes.
 *
 * FIXME: The horizontal alignment of the mora column is bad (too far
 * to the left), if the last dotted note is not the last primitive in
 * the ligature.  Fortunately, in practice this bug should have no
 * negative impact, since dotted notes appear within a ligature
 * usually always at the end of the ligature, such that the bug never
 * should apply for valid ligatures.
 *
 * TODO: Graduale Triplex, tempus per annum, hebdomada septima,
 * alleluia (page 280) shows a counter-example for collecting dots
 * always in a single column behind the ligature.  Maybe only the last
 * two dots in a ligature should be collected and all other dots put
 * behind or on top of the head?
 */
class Vaticana_ligature_engraver : public Gregorian_ligature_engraver
{

private:
  static bool need_extra_horizontal_space (int prev_prefix_set, int prefix_set,
                                           int context_info, int delta_pitch);
  bool is_stacked_head (int prefix_set, int context_info);
  Real align_heads (vector<Item *> const &primitives, Real flexa_width,
                    Real thickness);
  void check_for_prefix_loss (Item *primitive);
  void check_for_ambiguous_dot_pitch (Item *primitive);
  void add_mora_column (Paper_column *column);
  vector<Item *> augmented_primitives_;

public:
  TRANSLATOR_DECLARATIONS (Vaticana_ligature_engraver);

protected:
  Spanner *create_ligature_spanner () override;
  void transform_heads (Spanner *ligature,
                        vector<Item *> const &primitives) override;
};

Vaticana_ligature_engraver::Vaticana_ligature_engraver (Context *c)
  : Gregorian_ligature_engraver (c)
{
  brew_ligature_primitive_proc
    = Vaticana_ligature::brew_ligature_primitive_proc;
  augmented_primitives_.clear ();
}

Spanner *
Vaticana_ligature_engraver::create_ligature_spanner ()
{
  return make_spanner ("VaticanaLigature", SCM_EOL);
}

bool
Vaticana_ligature_engraver::is_stacked_head (int prefix_set, int context_info)
{
  bool is_stacked;

  // upper head of pes is stacked upon lower head of pes ...
  is_stacked = context_info & PES_UPPER;

  // ... unless this note starts a flexa
  if (context_info & FLEXA_LEFT)
    is_stacked = false;

  // ... or another pes
  if (context_info & PES_LOWER)
    is_stacked = false;

  // ... or the previous note is a semivocalis or inclinatum
  if (context_info & AFTER_DEMINUTUM)
    is_stacked = false;

  // auctum head is never stacked upon preceding note
  if (prefix_set & AUCTUM)
    is_stacked = false;

  // virga is never stacked upon preceding note
  if (prefix_set & VIRGA)
    is_stacked = false;

  // oriscus is never stacked upon preceding note
  if (prefix_set & ORISCUS)
    is_stacked = false;

  if ((prefix_set & DEMINUTUM) && !(prefix_set & INCLINATUM)
      && (context_info & FLEXA_RIGHT))
    is_stacked = true; // semivocalis head of deminutus form

  return is_stacked;
}

/*
 * When aligning the heads, sometimes extra space is needed, e.g. to
 * avoid clashing with the appendix of an adjacent notehead or with an
 * adjacent notehead itself if it has the same pitch.  Extra space is
 * added at most once between to heads.
 */
bool
Vaticana_ligature_engraver::need_extra_horizontal_space (int prev_prefix_set,
                                                         int prefix_set,
                                                         int context_info,
                                                         int delta_pitch)
{
  if (prev_prefix_set & VIRGA)
    /*
     * After a virga, make an additional small space such that the
     * appendix on the right side of the head does not touch the
     * following head.
     */
    return true;

  if ((prefix_set & INCLINATUM) && !(prev_prefix_set & INCLINATUM))
    /*
     * Always start a series of inclinatum heads with an extra space.
     */
    return true;

  if ((context_info & FLEXA_LEFT) && !(context_info & PES_UPPER))
    /*
     * Before a flexa (but not within a torculus), make an
     * additional small space such that the appendix on the left side
     * of the flexa does not touch the this head.
     */
    return true;

  if (delta_pitch == 0)
    /*
     * If there are two adjacent noteheads with the same pitch, add
     * additional small space between them, such that they do not
     * touch each other.
     */
    return true;

  return false;
}

Real
Vaticana_ligature_engraver::align_heads (vector<Item *> const &primitives,
                                         Real flexa_width, Real thickness)
{
  if (!primitives.size ())
    {
      programming_error ("Vaticana_ligature:"
                         " empty ligature [ignored]");
      return 0.0;
    }

  /*
   * The paper column where we put the whole ligature into.
   */
  auto *const column = primitives[0]->get_column ();

  Real join_thickness
    = thickness
      * column->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));

  /*
   * Amount of extra space two put between some particular
   * configurations of adjacent heads.
   *
   * TODO: make this a property of primtive grobs.
   */
  Real extra_space = 4.0 * join_thickness;

  /*
   * Keep track of the total width of the ligature.
   */
  Real ligature_width = 0.0;

  Item *prev_primitive = 0;
  int prev_prefix_set = 0;
  for (const auto &primitive : primitives)
    {
      int prefix_set = from_scm<int> (get_property (primitive, "prefix-set"));
      int context_info
        = from_scm<int> (get_property (primitive, "context-info"));

      /*
       * Get glyph_name, delta_pitch and context_info for this head.
       */

      SCM glyph_name_scm = get_property (primitive, "glyph-name");
      if (scm_is_null (glyph_name_scm))
        {
          primitive->programming_error ("Vaticana_ligature:"
                                        " undefined glyph-name ->"
                                        " ignoring grob");
          continue;
        }
      string glyph_name = ly_scm2string (glyph_name_scm);

      int delta_pitch = 0;
      if (prev_primitive) /* urgh, need prev_primitive only here */
        {
          SCM delta_pitch_scm = get_property (prev_primitive, "delta-position");
          if (!scm_is_null (delta_pitch_scm))
            delta_pitch = from_scm<int> (delta_pitch_scm);
          else
            {
              primitive->programming_error ("Vaticana_ligature:"
                                            " delta-position undefined ->"
                                            " ignoring grob");
              continue;
            }
        }

      /*
       * Now determine width and x-offset of head.
       */

      Real head_width;
      Real x_offset;

      if (context_info & STACKED_HEAD)
        {
          /*
           * This head is stacked upon the previous one; hence, it
           * does not contribute to the total width of the ligature,
           * and its width is assumed to be 0.0.  Moreover, it is
           * shifted to the left by its width such that the right side
           * of this and the other head are horizontally aligned.
           */
          head_width = 0.0;
          x_offset = join_thickness
                     - Font_interface::get_default_font (primitive)
                         ->find_by_name ("noteheads.s" + glyph_name)
                         .extent (X_AXIS)
                         .length ();
        }
      else if (glyph_name == "flexa" || glyph_name == "")
        {
          /*
           * This head represents either half of a flexa shape.
           * Hence, it is assigned half the width of this shape.
           */
          head_width = 0.5 * flexa_width;
          x_offset = 0.0;
        }
      else
        {
          /*
           * This is a regular head, placed right to the previous one.
           * Retrieve its width from corresponding font.
           */
          head_width = Font_interface::get_default_font (primitive)
                         ->find_by_name ("noteheads.s" + glyph_name)
                         .extent (X_AXIS)
                         .length ();
          x_offset = 0.0;
        }

      /*
       * Save the head's final x-offset.
       */
      set_property (primitive, "x-offset", to_scm (x_offset));

      /*
       * If the head is the 2nd head of a pes or flexa (but not a
       * flexa shape), mark this head to be joined with the left-side
       * neighbour head (i.e. the previous head) by a vertical beam.
       */
      if ((context_info & PES_UPPER)
          || ((context_info & FLEXA_RIGHT) && !(context_info & PES_LOWER)))
        {
          if (!prev_primitive)
            {
              primitive->programming_error ("Vaticana ligature: add-join:"
                                            " missing previous primitive");
            }
          else
            {
              set_property (prev_primitive, "add-join", SCM_BOOL_T);

              /*
               * Create a small overlap of adjacent heads so that the join
               * can be drawn perfectly between them.
               */
              ligature_width -= join_thickness;
            }
        }
      else if (glyph_name == "")
        {
          /*
           * This is the 2nd (virtual) head of flexa shape.  Join it
           * tightly with 1st head, i.e. do *not* add additional
           * space, such that next head will not be off from the flexa
           * shape.
           */
        }

      if (need_extra_horizontal_space (prev_prefix_set, prefix_set,
                                       context_info, delta_pitch))
        ligature_width += extra_space;

      /*
       * Horizontally line-up this head to form a ligature.
       */
      move_related_items_to_column (primitive, column, ligature_width);
      ligature_width += head_width;

      prev_primitive = primitive;
      prev_prefix_set = prefix_set;
    }

  /*
   * Add extra horizontal padding space after ligature, such that
   * neighbouring ligatures do not touch each other.
   */
  ligature_width += extra_space;

  return ligature_width;
}

/*
 * Depending on the typographical features of a particular ligature
 * style, some prefixes may be ignored.  In particular, if a curved
 * flexa shape is produced, any prefixes to either of the two
 * contributing heads that would select a head other than punctum, is
 * by definition ignored.
 *
 * This function prints a warning, if the given primitive is prefixed
 * such that a head other than punctum would be chosen, if this
 * primitive were engraved as a stand-alone head.
 */
void
Vaticana_ligature_engraver::check_for_prefix_loss (Item *primitive)
{
  int prefix_set = from_scm<int> (get_property (primitive, "prefix-set"));
  if (prefix_set & ~PES_OR_FLEXA)
    {
      string prefs = Gregorian_ligature::prefixes_to_str (primitive);
      primitive->warning (_f ("ignored prefix(es) `%s' of this head"
                              " according to restrictions of the selected"
                              " ligature style",
                              prefs.c_str ()));
    }
}

void
Vaticana_ligature_engraver::add_mora_column (Paper_column *column)
{
  if (augmented_primitives_.size () == 0) // no dot for column
    return;
  if (!column) // empty ligature???
    {
      augmented_primitives_[0]->programming_error (
        "no paper column to add dot");
      return;
    }
  Item *dotcol = make_item ("DotColumn", SCM_EOL);
  dotcol->set_x_parent (column);
  for (const auto &primitive : augmented_primitives_)
    {
      Item *dot = make_item ("Dots", primitive->self_scm ());
      set_property (dot, "dot-count", to_scm (1));
      dot->set_y_parent (primitive);
      set_object (primitive, "dot", dot->self_scm ());
      Dot_column::add_head (dotcol, primitive);

      // FIXME: why isn't the dot picked up by Paper_column_engraver?
      Separation_item::add_item (column, dot);
    }
}

/*
 * This function prints a warning, if the given primitive has the same
 * pitch as at least one of the primitives already stored in the
 * augmented_primitives_ array.
 *
 * The rationale of this check is, that, if there are two dotted
 * primitives with the same pitch, then collecting all dots in a dot
 * column behind the ligature leads to a notational ambiguity of to
 * which head the corresponding dot refers.
 *
 * Such a case should be treated as a badly specified ligature.  The
 * user should split the ligature to make the notation of dots
 * unambiguous.
 */
void
Vaticana_ligature_engraver::check_for_ambiguous_dot_pitch (Item *primitive)
{
  // TODO: Fix performance, which is currently O (n^2) (since this
  // method is called O (n) times and takes O (n) steps in the for
  // loop), but could be O (n) (by replacing the for loop by e.g. a
  // bitmask based O (1) test); where n=<number of primitives in the
  // ligature> (which is typically small (n<10), though).
  Stream_event *new_cause = primitive->event_cause ();
  int new_pitch = unsmob<Pitch> (get_property (new_cause, "pitch"))->steps ();
  for (vsize i = 0; i < augmented_primitives_.size (); i++)
    {
      Stream_event *cause = augmented_primitives_[i]->event_cause ();
      int pitch = unsmob<Pitch> (get_property (cause, "pitch"))->steps ();
      if (pitch == new_pitch)
        {
          primitive->warning (_ ("Ambiguous use of dots in ligature: there are"
                                 " multiple dotted notes with the same pitch."
                                 "  The ligature should be split."));
          return; // supress multiple identical warnings
        }
    }
}

void
Vaticana_ligature_engraver::transform_heads (Spanner *ligature,
                                             vector<Item *> const &primitives)
{
  Real flexa_width
    = from_scm<double> (get_property (ligature, "flexa-width"), 2);

  Real thickness = from_scm<double> (get_property (ligature, "thickness"), 1);

  Item *prev_primitive = 0;
  int prev_prefix_set = 0;
  int prev_context_info = 0;
  int prev_delta_pitch = 0;
  string prev_glyph_name = "";
  augmented_primitives_.clear ();
  for (vsize i = 0; i < primitives.size (); i++)
    {
      auto *const primitive = primitives[i];

      int delta_pitch;
      SCM delta_pitch_scm = get_property (primitive, "delta-position");
      if (!scm_is_null (delta_pitch_scm))
        delta_pitch = from_scm<int> (delta_pitch_scm);
      else
        {
          primitive->programming_error ("Vaticana_ligature:"
                                        " delta-position undefined ->"
                                        " ignoring grob");
          continue;
        }

      /* retrieve & complete prefix_set and context_info */
      int prefix_set = from_scm<int> (get_property (primitive, "prefix-set"));
      int context_info
        = from_scm<int> (get_property (primitive, "context-info"));

      if (Rhythmic_head::dot_count (primitive) > 0)
        // remove dots from primitive and add remember primitive for
        // creating a dot column
        {
          set_property (Rhythmic_head::get_dots (primitive), "dot-count",
                        to_scm (0));
          // TODO: Maybe completely remove grob "Dots" (dots->suicide
          // () ?) rather than setting property "dot-count" to 0.

          check_for_ambiguous_dot_pitch (primitives[i]);
          augmented_primitives_.push_back (primitives[i]);
        }
      else if (augmented_primitives_.size () > 0)
        {
          primitive->warning (_ ("This ligature has a dotted head followed by"
                                 " a non-dotted head.  The ligature should be"
                                 " split after the last dotted head before"
                                 " this head."));
        }

      if (is_stacked_head (prefix_set, context_info))
        {
          context_info |= STACKED_HEAD;
          set_property (primitive, "context-info", to_scm (context_info));
        }

      /*
       * Now determine which head to typeset (this is context sensitive
       * information, since it depends on neighbouring heads; therefore,
       * this decision must be made here in the engraver rather than in
       * the backend).
       */
      string glyph_name;
      if (prefix_set & VIRGA)
        {
          glyph_name = "vaticana.punctum";
          set_property (primitive, "add-stem", SCM_BOOL_T);
        }
      else if (prefix_set & QUILISMA)
        glyph_name = "vaticana.quilisma";
      else if (prefix_set & ORISCUS)
        glyph_name = "solesmes.oriscus";
      else if (prefix_set & STROPHA)
        if (prefix_set & AUCTUM)
          glyph_name = "solesmes.stropha.aucta";
        else
          glyph_name = "solesmes.stropha";
      else if (prefix_set & INCLINATUM)
        if (prefix_set & AUCTUM)
          glyph_name = "solesmes.incl.auctum";
        else if (prefix_set & DEMINUTUM)
          glyph_name = "solesmes.incl.parvum";
        else
          glyph_name = "vaticana.inclinatum";
      else if (prefix_set & DEMINUTUM)
        {
          if (i == 0)
            {
              // initio debilis
              glyph_name = "vaticana.reverse.plica";
            }
          else if (prev_delta_pitch > 0)
            {
              // epiphonus
              if (!(prev_context_info & FLEXA_RIGHT))
                {
                  /* correct head of previous primitive */
                  if (prev_delta_pitch > 1)
                    prev_glyph_name = "vaticana.epiphonus";
                  else
                    prev_glyph_name = "vaticana.vepiphonus";
                }
              if (prev_delta_pitch > 1)
                glyph_name = "vaticana.plica";
              else
                glyph_name = "vaticana.vplica";
            }
          else if (prev_delta_pitch < 0)
            {
              // cephalicus
              if (!(prev_context_info & FLEXA_RIGHT))
                /* correct head of previous primitive */
                {
                  if (i > 1)
                    {
                      /* cephalicus head with fixed size cauda */
                      prev_glyph_name = "vaticana.inner.cephalicus";
                    }
                  else
                    {
                      /* cephalicus head without cauda */
                      prev_glyph_name = "vaticana.cephalicus";
                    }

                  /*
                   * Flexa has no variable size cauda if its left head is
                   * stacked on the right head.  This is true for
                   * cephalicus.  Hence, remove the cauda.
                   *
                   * Urgh: for the current implementation, this rule only
                   * applies for cephalicus; but it is a fundamental rule.
                   * Therefore, the following line of code should be
                   * placed somewhere else.
                   */
                  set_property (prev_primitive, "add-cauda", SCM_BOOL_F);
                }
              if (prev_delta_pitch < -1)
                glyph_name = "vaticana.reverse.plica";
              else
                glyph_name = "vaticana.reverse.vplica";
            }
          else // (prev_delta_pitch == 0)
            {
              primitive->programming_error (
                "Vaticana_ligature:"
                " deminutum head must have different"
                " pitch -> ignoring grob");
            }
        }
      else if (prefix_set & (CAVUM | LINEA))
        if ((prefix_set & CAVUM) && (prefix_set & LINEA))
          glyph_name = "vaticana.linea.punctum.cavum";
        else if (prefix_set & CAVUM)
          glyph_name = "vaticana.punctum.cavum";
        else
          glyph_name = "vaticana.linea.punctum";
      else if (prefix_set & AUCTUM)
        if (prefix_set & ASCENDENS)
          glyph_name = "solesmes.auct.asc";
        else
          glyph_name = "solesmes.auct.desc";
      else if ((context_info & STACKED_HEAD) && (context_info & PES_UPPER))
        if (prev_delta_pitch > 1)
          glyph_name = "vaticana.upes";
        else
          glyph_name = "vaticana.vupes";
      else
        glyph_name = "vaticana.punctum";

      /*
       * This head needs a cauda, if it starts a flexa, is not the upper
       * head of a pes, and if it is a punctum.
       */
      if ((context_info & FLEXA_LEFT) && !(context_info & PES_UPPER))
        if (glyph_name == "vaticana.punctum")
          set_property (primitive, "add-cauda", SCM_BOOL_T);

      /*
       * Execptional rule for porrectus:
       *
       * If the current head is preceded by a \flexa and succeded by a
       * \pes (e.g. "a \flexa g \pes a"), then join the current head and
       * the previous head into a single curved flexa shape.
       */
      if ((context_info & FLEXA_RIGHT) && (context_info & PES_LOWER))
        {
          check_for_prefix_loss (prev_primitive);
          prev_glyph_name = "flexa";
          set_property (prev_primitive, "flexa-height",
                        to_scm (prev_delta_pitch));
          set_property (prev_primitive, "flexa-width", to_scm (flexa_width));
          bool add_cauda = !(prev_prefix_set & PES_OR_FLEXA);
          set_property (prev_primitive, "add-cauda", to_scm (add_cauda));
          check_for_prefix_loss (primitive);
          glyph_name = "";
          set_property (primitive, "flexa-width", to_scm (flexa_width));
        }

      /*
       * Exceptional rule for pes:
       *
       * If this head is stacked on the previous one due to a \pes, then
       * set the glyph of the previous head to that for this special
       * case, thereby avoiding potential vertical collision with the
       * current head.
       */
      if (prefix_set & PES_OR_FLEXA)
        {
          if ((context_info & PES_UPPER) && (context_info & STACKED_HEAD))
            {
              if (prev_glyph_name == "vaticana.punctum")
                {
                  if (prev_delta_pitch > 1)
                    prev_glyph_name = "vaticana.lpes";
                  else
                    prev_glyph_name = "vaticana.vlpes";
                }
            }
        }

      if (prev_primitive)
        set_property (prev_primitive, "glyph-name",
                      ly_string2scm (prev_glyph_name));

      /*
       * In the backend, flexa shapes and joins need to know about line
       * thickness.  Hence, for simplicity, let's distribute the
       * ligature grob's value for thickness to each ligature head (even
       * if not all of them need to know).
       */
      set_property (primitive, "thickness", to_scm (thickness));

      prev_primitive = primitive;
      prev_prefix_set = prefix_set;
      prev_context_info = context_info;
      prev_delta_pitch = delta_pitch;
      prev_glyph_name = glyph_name;
    }

  set_property (prev_primitive, "glyph-name", ly_string2scm (prev_glyph_name));

  align_heads (primitives, flexa_width, thickness);

  // append all dots to paper column of ligature's last head
  add_mora_column (prev_primitive->get_column ());
}

void
Vaticana_ligature_engraver::boot ()
{
  ADD_LISTENER (pes_or_flexa);
  ADD_DELEGATE_LISTENER (ligature);
  ADD_ACKNOWLEDGER (rest);
  ADD_ACKNOWLEDGER (ligature_head);
}

ADD_TRANSLATOR (Vaticana_ligature_engraver,
                /* doc */
                R"(
Handle ligatures by glueing special ligature heads together.
                )",

                /* create */
                R"(
VaticanaLigature
DotColumn
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
