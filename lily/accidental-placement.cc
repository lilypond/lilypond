/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "accidental-placement.hh"

#include "accidental-interface.hh"
#include "item.hh"
#include "ly-scm-list.hh"
#include "ly-smob-list.hh"
#include "music.hh"
#include "note-collision.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "rhythmic-head.hh"
#include "skyline.hh"
#include "skyline-pair.hh"
#include "stream-event.hh"
#include "warn.hh"

#include <algorithm>
#include <functional>
#include <memory>
#include <vector>

using std::unique_ptr;
using std::vector;

static Pitch *
accidental_pitch (Grob *acc)
{
  Stream_event *mcause = acc->get_y_parent ()->event_cause ();

  if (!mcause)
    {
      programming_error ("note head has no event cause");
      return 0;
    }

  return unsmob<Pitch> (get_property (mcause, "pitch"));
}

void
Accidental_placement::add_accidental (Grob *me, Grob *a, bool stagger,
                                      const void *hash_key)
{
  Pitch *p = accidental_pitch (a);
  if (!p)
    return;

  a->set_x_parent (me);

  SCM accs = get_object (me, "accidental-grobs");
  SCM key
    = scm_cons (to_scm (p->get_notename ()),
                to_scm (stagger ? std::hash<const void *> () (hash_key) : 1));
  // assoc because we're dealing with pairs
  SCM entry = ly_assoc (key, accs);
  if (scm_is_false (entry))
    entry = SCM_EOL;
  else
    entry = scm_cdr (entry);

  entry = scm_cons (a->self_scm (), entry);

  accs = scm_assoc_set_x (accs, key, entry);

  set_object (me, "accidental-grobs", accs);
}

/*
  Split into break reminders.
*/
void
Accidental_placement::split_accidentals (Grob *accs,
                                         vector<Grob *> *break_reminder,
                                         vector<Grob *> *real_acc)
{
  for (SCM entry : ly_scm_list (get_object (accs, "accidental-grobs")))
    for (auto *a : ly_smob_list<Grob> (scm_cdr (entry)))
      {
        if (unsmob<Grob> (get_object (a, "tie"))
            && !from_scm<bool> (get_property (a, "forced")))
          break_reminder->push_back (a);
        else
          real_acc->push_back (a);
      }
}

vector<Grob *>
Accidental_placement::get_relevant_accidentals (vector<Grob *> const &elts,
                                                Grob *left)
{
  vector<Grob *> br;
  vector<Grob *> ra;
  vector<Grob *> ret;
  bool right = dynamic_cast<Item *> (left)->break_status_dir () == RIGHT;

  for (vsize i = 0; i < elts.size (); i++)
    {
      split_accidentals (elts[i], &br, &ra);

      ret.insert (ret.end (), ra.begin (), ra.end ());

      if (right)
        ret.insert (ret.end (), br.begin (), br.end ());
    }
  return ret;
}

struct Accidental_placement_entry
{
  Skyline_pair horizontal_skylines_;
  vector<Grob *> grobs_;
};

Real
ape_priority (Accidental_placement_entry const *a)
{
  // right is up because we're horizontal
  return a->horizontal_skylines_.right ();
}

bool
ape_less (unique_ptr<Accidental_placement_entry> const &a,
          unique_ptr<Accidental_placement_entry> const &b)
{
  vsize size_a = a->grobs_.size ();
  vsize size_b = b->grobs_.size ();
  if (size_a != size_b)
    return size_b < size_a;

  return ape_priority (a.get ()) < ape_priority (b.get ());
}

/*
  This function provides a method for sorting accidentals that belong to the
  same note. The accidentals that this function considers to be "smallest"
  will be placed to the left of the "larger" accidentals.

  Naturals are the largest (so that they don't get confused with cancellation
  naturals); apart from that, we order according to the alteration (so
  double-flats are the smallest).

  Precondition: the accidentals are attached to NoteHeads of the same note
  name -- the octaves, however, may be different.
*/
static bool
acc_less (Grob *const &a, Grob *const &b)
{
  Pitch *p = accidental_pitch (a);
  Pitch *q = accidental_pitch (b);

  if (!p || !q)
    {
      programming_error ("these accidentals do not have a pitch");
      return false;
    }

  if (p->get_octave () != q->get_octave ())
    return p->get_octave () < q->get_octave ();

  if (p->get_alteration () == Rational (0))
    return false;
  if (q->get_alteration () == Rational (0))
    return true;

  return p->get_alteration () < q->get_alteration ();
}

/*
  TODO: should favor

  *  b
  * b

  placement
*/
void
stagger_apes (vector<unique_ptr<Accidental_placement_entry>> *apes)
{
  std::sort (apes->begin (), apes->end (), &ape_less);
  // we do the staggering below based on size
  // this ensures that if a placement has 4 entries, it will
  // always be closer to the NoteColumn than a placement with 1
  // this allows accidentals to be on-average closer to notes
  // while still preserving octave alignment
  vector<vector<unique_ptr<Accidental_placement_entry>>> ascs;

  vsize sz = INT_MAX;
  for (vsize i = 0; i < apes->size (); i++)
    {
      auto a = std::move ((*apes)[i]);
      vsize my_sz = a->grobs_.size ();
      if (sz != my_sz)
        ascs.push_back ({}); // empty vector
      ascs.back ().push_back (std::move (a));
      sz = my_sz;
    }

  apes->clear ();

  for (vsize i = 0; i < ascs.size (); i++)
    {
      int parity = 1;
      for (vsize j = 0; j < ascs[i].size ();)
        {
          unique_ptr<Accidental_placement_entry> a;
          if (parity)
            {
              a = std::move (ascs[i].back ());
              ascs[i].pop_back ();
            }
          else
            a = std::move (ascs[i][j++]);

          apes->push_back (std::move (a));
          parity = !parity;
        }
    }

  std::reverse (apes->begin (), apes->end ());
}

static vector<unique_ptr<Accidental_placement_entry>>
build_apes (SCM accs)
{
  vector<unique_ptr<Accidental_placement_entry>> apes;
  for (SCM entry : as_ly_scm_list (accs))
    {
      auto ape = std::make_unique<Accidental_placement_entry> ();
      for (auto *g : ly_smob_list<Grob> (scm_cdr (entry)))
        ape->grobs_.push_back (g);

      apes.push_back (std::move (ape));
    }

  return apes;
}

static void
set_ape_skylines (Accidental_placement_entry *ape, Grob **common, Real padding)
{
  vector<Grob *> accs (ape->grobs_);
  std::sort (accs.begin (), accs.end (), &acc_less);

  /* We know that each accidental has the same note name and we assume that
     accidentals in different octaves won't collide. If two or more
     accidentals are in the same octave:
     1) if they are the same accidental, print them in overstrike
     2) otherwise, shift one to the left so they don't overlap. */
  int last_octave = 0;
  Real offset = 0;
  Real last_offset = 0;
  Rational last_alteration (0);
  for (vsize i = accs.size (); i--;)
    {
      Grob *a = accs[i];
      Pitch *p = accidental_pitch (a);

      if (!p)
        continue;

      if (i == accs.size () - 1 || p->get_octave () != last_octave)
        {
          last_offset = 0;
          offset = a->extent (a, X_AXIS)[LEFT] - padding;
        }
      else if (p->get_alteration () == last_alteration)
        a->translate_axis (last_offset, X_AXIS);
      else /* Our alteration is different from the last one */
        {
          Real this_offset = offset - a->extent (a, X_AXIS)[RIGHT];
          a->translate_axis (this_offset, X_AXIS);

          last_offset = this_offset;
          offset -= a->extent (a, X_AXIS).length () + padding;
        }

      Skyline_pair skyps
        = from_scm<Skyline_pair> (get_property (a, "horizontal-skylines"));
      skyps.raise (a->relative_coordinate (common[X_AXIS], X_AXIS));
      skyps.shift (a->relative_coordinate (common[Y_AXIS], Y_AXIS));
      ape->horizontal_skylines_.merge (skyps);

      last_octave = p->get_octave ();
      last_alteration = p->get_alteration ();
    }
}

static vector<Grob *>
extract_heads_and_stems (
  vector<unique_ptr<Accidental_placement_entry>> const &apes)
{
  vector<Grob *> note_cols;
  vector<Grob *> ret;

  for (vsize i = apes.size (); i--;)
    {
      Accidental_placement_entry *ape = apes[i].get ();
      for (vsize j = ape->grobs_.size (); j--;)
        {
          Grob *acc = ape->grobs_[j];
          Grob *head = acc->get_y_parent ();
          Grob *col = head->get_x_parent ();

          if (has_interface<Note_column> (col))
            note_cols.push_back (col);
          else
            ret.push_back (head);
        }
    }

  /*
    This is a little kludgy: in case there are note columns without
    accidentals, we get them from the Note_collision objects.
  */
  for (vsize i = note_cols.size (); i--;)
    {
      Grob *c = note_cols[i]->get_x_parent ();
      if (has_interface<Note_collision_interface> (c))
        {
          extract_grob_set (c, "elements", columns);
          note_cols.insert (note_cols.end (), columns.begin (), columns.end ());
        }
    }

  /* Now that we have all of the columns, grab all of the note-heads */
  for (vsize i = note_cols.size (); i--;)
    {
      const auto &note_heads = extract_grob_array (note_cols[i], "note-heads");
      ret.insert (ret.end (), note_heads.begin (), note_heads.end ());
    }

  /* Now that we have all of the heads, grab all of the stems */
  for (vsize i = ret.size (); i--;)
    if (Grob *s = Rhythmic_head::get_stem (ret[i]))
      ret.push_back (s);

  uniquify (ret);
  return ret;
}

static Grob *
common_refpoint_of_accidentals (
  vector<unique_ptr<Accidental_placement_entry>> const &apes, Axis a)
{
  Grob *ret = 0;

  for (vsize i = apes.size (); i--;)
    for (vsize j = apes[i]->grobs_.size (); j--;)
      {
        if (!ret)
          ret = apes[i]->grobs_[j];
        else
          ret = ret->common_refpoint (apes[i]->grobs_[j], a);
      }

  return ret;
}

static Skyline
build_heads_skyline (vector<Grob *> const &heads_and_stems, Grob **common)
{
  vector<Box> head_extents;
  for (vsize i = heads_and_stems.size (); i--;)
    head_extents.push_back (
      Box (heads_and_stems[i]->extent (common[X_AXIS], X_AXIS),
           heads_and_stems[i]->pure_y_extent (common[Y_AXIS], 0, INT_MAX)));

  return Skyline (head_extents, Y_AXIS, LEFT);
}

/*
  Position the apes, starting from the right, so that they don't collide.
  Return the total width.
*/
static Interval
position_apes (Grob *me,
               vector<unique_ptr<Accidental_placement_entry>> const &apes,
               Skyline const &heads_skyline)
{
  Real padding = from_scm<double> (get_property (me, "padding"), 0.2);
  Skyline left_skyline = heads_skyline;
  left_skyline.raise (
    -from_scm<double> (get_property (me, "right-padding"), 0));

  /*
    Add accs entries right-to-left.
  */
  Interval width;
  Real last_offset = 0.0;
  for (vsize i = apes.size (); i-- > 0;)
    {
      Accidental_placement_entry *ape = apes[i].get ();

      Real offset
        = -ape->horizontal_skylines_[RIGHT].distance (left_skyline, 0.1);
      if (std::isinf (offset))
        offset = last_offset;
      else
        offset -= padding;

      Skyline new_left_skyline = ape->horizontal_skylines_[LEFT];
      new_left_skyline.raise (offset);
      new_left_skyline.merge (left_skyline);
      left_skyline = new_left_skyline;

      /* Shift all of the accidentals in this ape */
      for (vsize j = ape->grobs_.size (); j--;)
        ape->grobs_[j]->translate_axis (offset, X_AXIS);

      for (const auto d : {LEFT, RIGHT})
        {
          Real mh = ape->horizontal_skylines_[d].max_height ();
          if (!std::isinf (mh))
            width.add_point (mh + offset);
        }

      last_offset = offset;
    }

  return width;
}

/*
  This routine computes placements of accidentals. During
  add_accidental (), accidentals are already grouped by note, so that
  octaves are placed above each other; they form columns. Then the
  columns are sorted: the biggest columns go closest to the note.
  Then the columns are spaced as closely as possible (using skyline
  spacing).


  TODO: more advanced placement. Typically, the accs should be placed
  to form a C shape, like this

  *     ##
  *  b b
  * # #
  *  b
  *    b b

  The naturals should be left of the C as well; they should
  be separate accs.

  Note that this placement problem looks NP hard, so we just use a
  simple strategy, not an optimal choice.
*/

/*
  TODO: there should be more space in the following situation


  Natural + downstem

  *
  *  |_
  *  | |    X
  *  |_|   |
  *    |   |
  *
*/

MAKE_SCHEME_CALLBACK (Accidental_placement, calc_positioning_done,
                      "ly:accidental-placement::calc-positioning-done", 1);
SCM
Accidental_placement::calc_positioning_done (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  if (!me->is_live ())
    return SCM_BOOL_T;

  set_property (me, "positioning-done", SCM_BOOL_T);

  SCM accs = get_object (me, "accidental-grobs");
  if (!scm_is_pair (accs))
    return SCM_BOOL_T;

  vector<unique_ptr<Accidental_placement_entry>> apes = build_apes (accs);

  Grob *common[] = {me, 0};

  vector<Grob *> heads_and_stems = extract_heads_and_stems (apes);

  common[Y_AXIS] = common_refpoint_of_accidentals (apes, Y_AXIS);
  common[Y_AXIS]
    = common_refpoint_of_array (heads_and_stems, common[Y_AXIS], Y_AXIS);
  common[X_AXIS] = common_refpoint_of_array (heads_and_stems, me, X_AXIS);
  Real padding = from_scm<double> (get_property (me, "padding"), 0.2);

  for (vsize i = apes.size (); i--;)
    set_ape_skylines (apes[i].get (), common, padding);
  Skyline heads_skyline = build_heads_skyline (heads_and_stems, common);

  stagger_apes (&apes);
  Interval width = position_apes (me, apes, heads_skyline);

  me->flush_extent_cache (X_AXIS);
  set_property (me, "X-extent", to_scm (width));

  return SCM_BOOL_T;
}

ADD_INTERFACE (Accidental_placement,
               R"(
Resolve accidental collisions.
               )",

               /* properties */
               R"(
accidental-grobs
direction
padding
positioning-done
right-padding
script-priority
               )");
