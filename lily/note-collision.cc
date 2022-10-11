/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "note-collision.hh"

#include "axis-group-interface.hh"
#include "dot-column.hh"
#include "international.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "item.hh"
#include "rhythmic-head.hh"
#include "staff-symbol-referencer.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "warn.hh"

#include <algorithm>
#include <vector>

using std::vector;

Real
check_meshing_chords (Grob *me, Grob *clash_up, Grob *clash_down)

{
  /* Every note column should have a stem, but avoid a crash. */
  if (!Note_column::get_stem (clash_up) || !Note_column::get_stem (clash_down))
    return 0.0;

  Drul_array<Grob *> stems (Note_column::get_stem (clash_down),
                            Note_column::get_stem (clash_up));

  Grob *head_up = Note_column::first_head (clash_up);
  Grob *head_down = Note_column::first_head (clash_down);

  Interval extent_up = head_up->extent (head_up, X_AXIS);
  Interval extent_down = head_down->extent (head_down, X_AXIS);

  /* Staff-positions of all noteheads on each stem */
  vector<int> ups = Stem::note_head_positions (stems[UP]);
  vector<int> dps = Stem::note_head_positions (stems[DOWN]);

  int threshold = from_scm (get_property (me, "note-collision-threshold"), 1);

  /* Too far apart to collide. */
  if (ups[0] > dps.back () + threshold)
    return 0.0;

  /* If the chords just 'touch' their extreme noteheads,
     then we can align their stems.
  */
  bool touch = false;
  if (ups[0] >= dps.back ()
      && (dps.size () < 2 || ups[0] >= dps[dps.size () - 2] + threshold + 1)
      && (ups.size () < 2 || ups[1] >= dps.back () + threshold + 1))
    touch = true;

  /* Filter out the 'o's in this configuration, since they're no
   * part in the collision.
   *
   *  |
   * x|o
   * x|o
   * x
   *
   */
  ups = Stem::note_head_positions (stems[UP], true);
  dps = Stem::note_head_positions (stems[DOWN], true);

  /* Merge heads if the notes lie the same line, or if the "stem-up-note" is
     above the "stem-down-note". */
  bool merge_possible = (ups[0] >= dps[0]) && (ups.back () >= dps.back ());

  /* Do not merge notes typeset in different style. */
  if (!ly_is_equal (get_property (head_up, "style"),
                    get_property (head_down, "style")))
    merge_possible = false;

  int up_ball_type = Rhythmic_head::duration_log (head_up);
  int down_ball_type = Rhythmic_head::duration_log (head_down);

  /* Do not merge whole notes (or longer, like breve, longa, maxima). */
  if (merge_possible && (up_ball_type <= 0 || down_ball_type <= 0))
    merge_possible = false;

  if (merge_possible
      && Rhythmic_head::dot_count (head_up)
           != Rhythmic_head::dot_count (head_down)
      && !from_scm<bool> (get_property (me, "merge-differently-dotted")))
    merge_possible = false;

  /* Can only merge different heads if merge-differently-headed is set. */
  if (merge_possible && up_ball_type != down_ball_type
      && !from_scm<bool> (get_property (me, "merge-differently-headed")))
    merge_possible = false;

  /* Should never merge quarter and half notes, as this would make
     them indistinguishable.  */
  if (merge_possible
      && ((Stem::duration_log (stems[UP]) == 1
           && Stem::duration_log (stems[DOWN]) == 2)
          || (Stem::duration_log (stems[UP]) == 2
              && Stem::duration_log (stems[DOWN]) == 1)))
    merge_possible = false;

  /*
   * this case (distant half collide),
   *
   *    |
   *  x |
   * | x
   * |
   *
   * the noteheads may be closer than this case (close half collide)
   *
   *    |
   *    |
   *   x
   *  x
   * |
   * |
   *
   */

  bool close_half_collide = false;
  bool distant_half_collide = false;
  bool full_collide = false;

  for (vsize i = 0, j = 0; i < ups.size () && j < dps.size ();)
    {
      if (ups[i] == dps[j])
        full_collide = true;
      else if (abs (ups[i] - dps[j]) <= threshold)
        {
          merge_possible = false;
          if (ups[i] > dps[j])
            close_half_collide = true;
          else
            distant_half_collide = true;
        }
      else if (ups[i] > dps[0] && ups[i] < dps.back ())
        merge_possible = false;
      else if (dps[j] > ups[0] && dps[j] < ups.back ())
        merge_possible = false;

      if (ups[i] < dps[j])
        i++;
      else if (ups[i] > dps[j])
        j++;
      else
        {
          i++;
          j++;
        }
    }

  full_collide = full_collide || (close_half_collide && distant_half_collide)
                 || (distant_half_collide // like full_ for wholes and longer
                     && (up_ball_type <= 0 || down_ball_type <= 0));

  /* Determine which chord goes on the left, and which goes right.
     Up-stem usually goes on the right, but if chords just 'touch' we can put
     both stems on a common vertical line.  In the presense of collisions,
     right hand heads may obscure dots, so dotted heads to go the right.
  */
  Real shift_amount = 1;
  bool stem_to_stem = false;
  if ((full_collide
       || ((close_half_collide || distant_half_collide)
           && from_scm<bool> (get_property (me, "prefer-dotted-right"))))
      && Rhythmic_head::dot_count (head_up)
           < Rhythmic_head::dot_count (head_down))
    {
      shift_amount = -1;
      if (!touch)
        // remember to leave clearance between stems
        stem_to_stem = true;
    }
  else if (touch)
    {
      // Up-stem note on a line has a raised dot, so no risk of collision
      Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
      if ((full_collide
           || (!Staff_symbol_referencer::on_line (staff, ups[0])
               && from_scm<bool> (get_property (me, "prefer-dotted-right"))))
          && Rhythmic_head::dot_count (head_up)
               > Rhythmic_head::dot_count (head_down))
        touch = false;
      else
        shift_amount = -1;
    }

  /* The 'fa' shape note heads have a triangular shape, which is
     inverted depending on the stem direction.  In case of a
     collision, one of them should be removed so that the resulting
     note does not look like a rectangular block.
  */
  SCM fa_styles = get_property (me, "fa-styles");
  SCM up_style = get_property (head_up, "style");
  SCM down_style = get_property (head_down, "style");
  if (merge_possible && scm_is_true (scm_memq (up_style, fa_styles))
      && scm_is_true (scm_memq (down_style, fa_styles)))
    {
      // Compute which shape should be displayed.
      Direction d = from_scm (get_property (me, "fa-merge-direction"), DOWN);

      // Hide unwanted glyph.
      set_property (d == UP ? head_down : head_up, "transparent", SCM_BOOL_T);

      // Adjust starting point of the stem to get a smooth connection
      // between stem and glyph.
      Offset up_att = Offset (0.0, d == UP ? 0.5 : -1.0);
      Offset down_att = Offset (0.0, d == DOWN ? -0.5 : 1.0);
      set_property (head_up, "stem-attachment", to_scm (up_att));
      set_property (head_down, "stem-attachment", to_scm (down_att));
    }

  if (merge_possible)
    {
      shift_amount = 0;

      /* If possible, don't wipe any heads.  Else, wipe shortest head,
         or head with smallest amount of dots.  Note: when merging
         different heads, dots on the smaller one disappear; and when
         merging identical heads, dots on the down-stem head disappear */
      Grob *wipe_ball = 0;
      Grob *dot_wipe_head = head_up;

      if (up_ball_type == down_ball_type)
        {
          if (Rhythmic_head::dot_count (head_down)
              < Rhythmic_head::dot_count (head_up))
            {
              wipe_ball = head_down;
              dot_wipe_head = head_down;
            }
          else if (Rhythmic_head::dot_count (head_down)
                   > Rhythmic_head::dot_count (head_up))
            {
              dot_wipe_head = head_up;
              wipe_ball = head_up;
            }
          else
            dot_wipe_head = head_down;
        }
      else if (down_ball_type > up_ball_type)
        {
          wipe_ball = head_down;
          dot_wipe_head = head_down;
        }
      else if (down_ball_type < up_ball_type)
        {
          wipe_ball = head_up;
          dot_wipe_head = head_up;
          /*
            If upper head is eighth note or shorter, and lower head is half note,
            shift by the difference between the open and filled note head widths,
            otherwise upper stem will be misaligned slightly.
          */
          if (Stem::duration_log (stems[DOWN]) == 1
              && Stem::duration_log (stems[UP]) >= 3)
            shift_amount = (1 - extent_up[RIGHT] / extent_down[RIGHT]) * 0.5;
        }

      if (dot_wipe_head)
        {
          if (Grob *d = unsmob<Grob> (get_object (dot_wipe_head, "dot")))
            d->suicide ();
        }

      if (wipe_ball && wipe_ball->is_live ())
        set_property (wipe_ball, "transparent", SCM_BOOL_T);
    }
  /* TODO: these numbers are magic; should devise a set of grob props
     to tune this behavior. */
  else if (stem_to_stem)
    shift_amount *= 0.65;
  else if (touch)
    shift_amount *= 0.5;
  else if (close_half_collide)
    shift_amount *= 0.52;
  else if (full_collide)
    shift_amount *= 0.5;
  else if (distant_half_collide)
    shift_amount *= 0.4;

  /* we're meshing. */
  else if (Rhythmic_head::dot_count (head_up)
           || Rhythmic_head::dot_count (head_down))
    shift_amount *= 0.1;
  else
    shift_amount *= 0.17;

  /* The offsets computed in this routine are multiplied,
     in calc_positioning_done(), by the width of the downstem note.
     The shift required to clear collisions, however, depends on the extents
     of the note heads on the sides that interfere. */
  if (shift_amount < 0.0) // Down-stem shifts right.
    shift_amount
      *= (extent_up[RIGHT] - extent_down[LEFT]) / extent_down.length ();
  else // Up-stem shifts right.
    shift_amount
      *= (extent_down[RIGHT] - extent_up[LEFT]) / extent_down.length ();

  /* If any dotted notes ended up on the left,
     tell the Dot_Columnn to avoid the note heads on the right.
   */
  if (shift_amount < -1e-6 && Rhythmic_head::dot_count (head_up))
    {
      Grob *d = unsmob<Grob> (get_object (head_up, "dot"));
      Grob *parent = d->get_x_parent ();
      if (has_interface<Dot_column> (parent))
        Side_position_interface::add_support (parent, head_down);
    }
  else if (Rhythmic_head::dot_count (head_down))
    {
      Grob *d = unsmob<Grob> (get_object (head_down, "dot"));
      Grob *parent = d->get_x_parent ();
      if (has_interface<Dot_column> (parent))
        {
          Grob *stem = unsmob<Grob> (get_object (head_up, "stem"));
          // Loop over all heads on an up-pointing-stem to see if dots
          // need to clear any heads suspended on its right side.
          extract_grob_set (stem, "note-heads", heads);
          for (vsize i = 0; i < heads.size (); i++)
            Side_position_interface::add_support (parent, heads[i]);
        }
    }

  // In meshed chords with dots on the left, adjust dot direction
  if (shift_amount > 1e-6 && Rhythmic_head::dot_count (head_down))
    {
      Grob *dot_down = unsmob<Grob> (get_object (head_down, "dot"));
      Grob *col_down = dot_down->get_x_parent ();
      Direction dir = UP;
      if (Rhythmic_head::dot_count (head_up))
        {
          Grob *dot_up = unsmob<Grob> (get_object (head_up, "dot"));
          Grob *col_up = dot_up->get_x_parent ();
          if (col_up == col_down) // let the common DotColumn arrange dots
            dir = CENTER;
          else // conform to the dot direction on the up-stem chord
            dir = from_scm (get_property (dot_up, "direction"), UP);
        }
      if (dir != CENTER)
        {
          Grob *stem = unsmob<Grob> (get_object (head_down, "stem"));
          extract_grob_set (stem, "note-heads", heads);
          for (vsize i = 0; i < heads.size (); i++)
            if (Grob *dot = unsmob<Grob> (get_object (heads[i], "dot")))
              set_property (dot, "direction", to_scm (dir));
        }
    }

  return shift_amount;
}

MAKE_SCHEME_CALLBACK (Note_collision_interface, calc_positioning_done,
                      "ly:note-collision-interface::calc-positioning-done", 1)
SCM
Note_collision_interface::calc_positioning_done (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  set_property (me, "positioning-done", SCM_BOOL_T);

  Drul_array<vector<Grob *>> clash_groups = get_clash_groups (me);

  for (const auto d : {UP, DOWN})
    {
      for (vsize i = clash_groups[d].size (); i--;)
        {
          /*
            Trigger positioning
          */
          clash_groups[d][i]->extent (me, X_AXIS);
        }
    }

  SCM autos (automatic_shift (me, clash_groups));
  SCM hand (forced_shift (me));

  Real wid = 0.0;
  for (const auto d : {UP, DOWN})
    {
      if (clash_groups[d].size ())
        {
          Grob *h = clash_groups[d][0];
          Grob *fh = Note_column::first_head (h);
          if (fh)
            wid = fh->extent (h, X_AXIS).length ();
        }
    }

  vector<Grob *> done;
  Real left_most = 0.0;

  vector<Real> amounts;
  for (; scm_is_pair (hand); hand = scm_cdr (hand))
    {
      Grob *s = unsmob<Grob> (scm_caar (hand));
      Real amount = from_scm<double> (scm_cdar (hand)) * wid;

      done.push_back (s);
      amounts.push_back (amount);
    }
  for (; scm_is_pair (autos); autos = scm_cdr (autos))
    {
      Grob *s = unsmob<Grob> (scm_caar (autos));
      Real amount = from_scm<double> (scm_cdar (autos)) * wid;

      auto it = std::find (done.begin (), done.end (), s);
      if (it == done.end ())
        {
          done.push_back (s);
          amounts.push_back (amount);
          if (amount < left_most)
            left_most = amount;
        }
    }

  for (vsize i = 0; i < amounts.size (); i++)
    done[i]->translate_axis (amounts[i] - left_most, X_AXIS);

  return SCM_BOOL_T;
}

Drul_array<vector<Grob *>>
Note_collision_interface::get_clash_groups (Grob *me)
{
  Drul_array<vector<Grob *>> clash_groups;

  extract_grob_set (me, "elements", elements);
  for (vsize i = 0; i < elements.size (); i++)
    {
      Grob *se = elements[i];
      if (has_interface<Note_column> (se))
        {
          if (!Note_column::dir (se))
            se->programming_error ("note-column has no direction");
          else
            clash_groups[Note_column::dir (se)].push_back (se);
        }
    }

  for (const auto d : {UP, DOWN})
    {
      vector<Grob *> &clashes (clash_groups[d]);
      std::sort (clashes.begin (), clashes.end (), Note_column::shift_less);
    }

  return clash_groups;
}

/*
  This complicated routine moves note columns around horizontally to
  ensure that notes don't clash.
*/
SCM
Note_collision_interface::automatic_shift (
  Grob *me, Drul_array<vector<Grob *>> clash_groups)
{
  SCM tups = SCM_EOL;

  Drul_array<vector<Slice>> extents;
  Drul_array<Slice> extent_union;
  Drul_array<vector<Grob *>> stems;
  for (const auto d : {UP, DOWN})
    {
      for (vsize i = 0; i < clash_groups[d].size (); i++)
        {
          Slice s (Note_column::head_positions_interval (clash_groups[d][i]));
          s[LEFT]--;
          s[RIGHT]++;
          extents[d].push_back (s);
          extent_union[d].unite (s);
          stems[d].push_back (Note_column::get_stem (clash_groups[d][i]));
        }
    }

  Real inner_offset
    = (clash_groups[UP].size () && clash_groups[DOWN].size ())
        ? check_meshing_chords (me, clash_groups[UP][0], clash_groups[DOWN][0])
        : 0.0;

  /*
   * do horizontal shifts of each direction
   *
   *  |
   * x||
   *  x||
   *   x|
  */
  Drul_array<vector<Real>> offsets;
  for (const auto d : {UP, DOWN})
    {
      Real offset = inner_offset;
      vector<int> shifts;
      for (vsize i = 0; i < clash_groups[d].size (); i++)
        {
          Grob *col = clash_groups[d][i];
          SCM sh = get_property (col, "horizontal-shift");
          shifts.push_back (from_scm (sh, 0));

          if (i == 0)
            offset = inner_offset;
          else
            {
              bool explicit_shift = scm_is_number (sh);
              if (!explicit_shift)
                col->warning (
                  _ ("this Voice needs a \\voiceXx or \\shiftXx setting"));

              if (explicit_shift && shifts[i] == shifts[i - 1])
                ; // Match the previous notecolumn offset
              else if (extents[d][i][UP] > extents[d][i - 1][DOWN]
                       && extents[d][i][DOWN] < extents[d][i - 1][UP])
                offset += 1.0; // fully clear the previous-notecolumn heads
              else if (d * extents[d][i][-d] >= d * extents[d][i - 1][d])
                offset += Stem::is_valid_stem (stems[d][i - 1])
                            ? 1.0
                            : 0.5; // we cross the previous notecolumn
              else if (Stem::is_valid_stem (stems[d][i]))
                offset += 0.5;

              // check if we cross the opposite-stemmed voices
              if (d * extents[d][i][-d] < d * extent_union[-d][d])
                offset = std::max (offset, 0.5);
              if (extents[-d].size ()
                  && extents[d][i][UP] > extents[-d][0][DOWN]
                  && extents[d][i][DOWN] < extents[-d][0][UP])
                offset = std::max (offset, 1.0);
            }
          offsets[d].push_back (d * offset);
        }
    }

  /*
    see input/regression/dot-up-voice-collision.ly
  */
  for (vsize i = 0; i < clash_groups[UP].size (); i++)
    {
      Grob *g = clash_groups[UP][i];
      Grob *dc = Note_column::dot_column (g);

      if (dc)
        for (vsize j = i + 1; j < clash_groups[UP].size (); j++)
          Side_position_interface::add_support (dc, stems[UP][j]);
    }

  for (const auto d : {UP, DOWN})
    {
      for (vsize i = 0; i < clash_groups[d].size (); i++)
        tups = scm_cons (
          scm_cons (clash_groups[d][i]->self_scm (), to_scm (offsets[d][i])),
          tups);
    }

  return tups;
}

SCM
Note_collision_interface::forced_shift (Grob *me)
{
  SCM tups = SCM_EOL;

  extract_grob_set (me, "elements", elements);
  for (vsize i = 0; i < elements.size (); i++)
    {
      Grob *se = elements[i];

      SCM force = get_property (se, "force-hshift");
      if (scm_is_number (force))
        tups = scm_cons (scm_cons (se->self_scm (), force), tups);
    }
  return tups;
}

void
Note_collision_interface::add_column (Grob *me, Grob *ncol)
{
  set_property (ncol, "X-offset", Grob::x_parent_positioning_proc);
  Axis_group_interface::add_element (me, ncol);
}

vector<int>
Note_collision_interface::note_head_positions (Grob *me)
{
  vector<int> out;
  extract_grob_set (me, "elements", elts);
  for (vsize i = 0; i < elts.size (); i++)
    if (Grob *stem = unsmob<Grob> (get_object (elts[i], "stem")))
      {
        vector<int> nhp = Stem::note_head_positions (stem);
        out.insert (out.end (), nhp.begin (), nhp.end ());
      }

  std::sort (out.begin (), out.end ());
  return out;
}

ADD_INTERFACE (Note_collision_interface,
               R"(
An object that handles collisions between notes with different stem directions
and horizontal shifts.  Most of the interesting properties are to be set in
@ref{note-column-interface}: these are @code{force-hshift} and
@code{horizontal-shift}.
               )",

               /* properties */
               R"(
fa-merge-direction
fa-styles
merge-differently-dotted
merge-differently-headed
note-collision-threshold
positioning-done
prefer-dotted-right
               )");
