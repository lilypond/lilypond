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

#include "break-align-interface.hh"

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "dimensions.hh"
#include "international.hh"
#include "ly-scm-list.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "warn.hh"

using std::string;
using std::vector;

/*
  This is tricky: we cannot modify 'elements, since callers are
  iterating the same list. Reordering the list in-place, or resetting
  'elements will skip elements in the loops of callers.

  So we return the correct order as an array.
*/
SCM
Break_alignment_interface::break_align_order (Item *me)
{
  if (!me)
    return SCM_BOOL_F;

  SCM order_vec = get_property (me, "break-align-orders");
  if (!scm_is_vector (order_vec)
      || scm_c_vector_length (order_vec) < 3)
    return SCM_BOOL_F;

  SCM order = scm_vector_ref (order_vec,
                              to_scm (me->break_status_dir ().to_index ()));

  return order;
}

vector<Grob *>
Break_alignment_interface::ordered_elements (Item *me)
{
  extract_grob_set (me, "elements", elts);

  SCM order = break_align_order (me);

  if (scm_is_false (order))
    return elts;

  vector<Grob *> writable_elts (elts);
  /*
   Copy in order specified in BREAK-ALIGN-ORDER.
  */
  vector<Grob *> new_elts;
  for (; scm_is_pair (order); order = scm_cdr (order))
    {
      SCM sym = scm_car (order);

      for (vsize i = writable_elts.size (); i--;)
        {
          Grob *g = writable_elts[i];
          if (g && scm_is_eq (sym, get_property (g, "break-align-symbol")))
            {
              new_elts.push_back (g);
              writable_elts.erase (writable_elts.begin () + i);
            }
        }
    }

  return new_elts;
}

void
Break_alignment_interface::add_element (Item *me, Item *toadd)
{
  Align_interface::add_element (me, toadd);
}

/* Main routine to space breakable items in one column
   according to space-alist specifications. */
MAKE_SCHEME_CALLBACK (Break_alignment_interface, calc_positioning_done, 1)
SCM
Break_alignment_interface::calc_positioning_done (SCM smob)
{
  auto *const me = unsmob<Item> (smob);

  set_property (me, "positioning-done", SCM_BOOL_T);

  vector<Grob *> const &elems = ordered_elements (me);

  vector<Interval> extents;
  for (Grob *g : elems)
    extents.push_back (g->extent (g, X_AXIS));

  vsize idx = 0;
  while (idx < extents.size () && extents[idx].is_empty ())
    idx++;

  vector<Real> offsets (elems.size (), 0.0);

  Real extra_right_space = 0.0;
  vsize edge_idx = VPOS;
  while (idx < elems.size ())
    {
      vsize next_idx = idx + 1;
      while (next_idx < elems.size ()
             && extents[next_idx].is_empty ())
        next_idx++;

      Grob *l = elems[idx];
      Grob *r = nullptr;

      if (next_idx < elems.size ())
        r = elems[next_idx];

      SCM alist = SCM_EOL;

      /*
        Find the first grob with a space-alist entry.
      */
      extract_grob_set (l, "elements", elts);

      for (vsize i = elts.size (); i--;)
        {
          Grob *elt = elts[i];

          if (edge_idx == VPOS
              && scm_is_eq (get_property (elt, "break-align-symbol"),
                            ly_symbol2scm ("left-edge")))
            edge_idx = idx;

          SCM l = get_property (elt, "space-alist");
          if (scm_is_pair (l))
            {
              alist = l;
              break;
            }
        }

      SCM rsym = r ? SCM_EOL : ly_symbol2scm ("right-edge");

      /*
        We used to use #'cause to find out the symbol and the spacing
        table, but that gets icky when that grob is suicided for some
        reason.
      */
      if (r)
        {
          extract_grob_set (r, "elements", elts);
          for (vsize i = elts.size ();
               !scm_is_symbol (rsym) && i--;)
            {
              Grob *elt = elts[i];
              rsym = get_property (elt, "break-align-symbol");
            }
        }

      if (scm_is_eq (rsym, ly_symbol2scm ("left-edge")))
        edge_idx = next_idx;

      SCM entry = SCM_EOL;
      if (scm_is_symbol (rsym))
        entry = scm_assq (rsym, alist);

      bool entry_found = scm_is_pair (entry);
      if (!entry_found)
        {
          string sym_string;
          if (scm_is_symbol (rsym))
            sym_string = ly_symbol2string (rsym);

          string orig_string;
          if (unsmob<Grob> (get_property (l, "cause")))
            orig_string = unsmob<Grob> (get_property (l, "cause"))->name ();

          programming_error (to_string ("No spacing entry from %s to `%s'",
                                        orig_string.c_str (),
                                        sym_string.c_str ()));
        }

      Real distance = 1.0;
      SCM type = ly_symbol2scm ("extra-space");

      if (entry_found)
        {
          entry = scm_cdr (entry);

          distance = from_scm<Real> (scm_cdr (entry));
          type = scm_car (entry);
        }

      if (r)
        {
          if (scm_is_eq (type, ly_symbol2scm ("extra-space")))
            offsets[next_idx] = extents[idx][RIGHT] + distance
                                - extents[next_idx][LEFT];
          /* should probably junk minimum-space */
          else if (scm_is_eq (type, ly_symbol2scm ("minimum-space")))
            offsets[next_idx] = std::max (extents[idx][RIGHT], distance);
        }
      else
        {
          extra_right_space = distance;
          if (idx + 1 < offsets.size ())
            offsets[idx + 1] = extents[idx][RIGHT] + distance;
        }

      idx = next_idx;
    }

  Real here = 0.0;
  Interval total_extent;

  Real alignment_off = 0.0;
  for (vsize i = 0; i < offsets.size (); i++)
    {
      here += offsets[i];
      if (i == edge_idx)
        alignment_off = -here;
      total_extent.unite (extents[i] + here);
    }

  if (total_extent.is_empty ())
    return SCM_BOOL_T;

  if (me->break_status_dir () == LEFT)
    alignment_off = -total_extent[RIGHT] - extra_right_space;
  else if (edge_idx == VPOS)
    alignment_off = -total_extent[LEFT];

  here = alignment_off;
  for (vsize i = 0; i < offsets.size (); i++)
    {
      here += offsets[i];
      elems[i]->translate_axis (here, X_AXIS);
    }

  return SCM_BOOL_T;
}

MAKE_SCHEME_CALLBACK (Break_alignable_interface, find_parent, 1)
SCM
Break_alignable_interface::find_parent (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const alignment_parent = find_parent (me);
  return alignment_parent ? alignment_parent->self_scm () : SCM_BOOL_F;
}

Item *
Break_alignable_interface::find_parent (Grob *me)
{
  auto *const alignment = dynamic_cast<Item *> (me->get_x_parent ());
  if (!has_interface<Break_alignment_interface> (alignment))
    return nullptr;

  auto elements = Break_alignment_interface::ordered_elements (alignment);
  if (elements.empty ())
    return nullptr;

  Item *break_aligned_grob = nullptr;
  SCM symbol_list = get_property (me, "break-align-symbols");
  for (SCM sym : as_ly_scm_list (symbol_list))
    {
      for (auto *g : elements)
        {
          if (scm_is_eq (sym, get_property (g, "break-align-symbol")))
            {
              // Someone would have to do something unusual in Scheme to get a
              // Spanner here.
              if (auto *it = dynamic_cast<Item *> (g))
                {
                  if (it->break_visible ()
                      // TODO SCM: simplify syntax?
                      && !it->extent (it, X_AXIS).is_empty ())
                    {
                      return it;
                    }
                  else if (!break_aligned_grob)
                    break_aligned_grob = it;
                }
            }
        }
    }

  return break_aligned_grob;
}

MAKE_SCHEME_CALLBACK (Break_alignable_interface, self_align_callback, 1)
SCM
Break_alignable_interface::self_align_callback (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  auto *const alignment_parent = find_parent (me);
  if (!alignment_parent)
    return to_scm (0);

  Grob *common = me->common_refpoint (alignment_parent, X_AXIS);
  Real anchor = from_scm<double> (get_property (alignment_parent, "break-align-anchor"), 0);

  return to_scm (alignment_parent->relative_coordinate (common, X_AXIS)
                 - me->relative_coordinate (common, X_AXIS)
                 + anchor);
}

MAKE_SCHEME_CALLBACK (Break_aligned_interface, calc_average_anchor, 1)
SCM
Break_aligned_interface::calc_average_anchor (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  Real avg = 0.0;
  int count = 0;

  /* average the anchors of those children that have it set */
  extract_grob_set (me, "elements", elts);
  for (vsize i = 0; i < elts.size (); i++)
    {
      SCM anchor = get_property (elts[i], "break-align-anchor");
      if (scm_is_number (anchor))
        {
          count++;
          avg += scm_to_double (anchor);
        }
    }

  return to_scm (count > 0 ? avg / count : 0);
}

MAKE_SCHEME_CALLBACK (Break_aligned_interface, calc_joint_anchor_alignment, 1)
SCM
Break_aligned_interface::calc_joint_anchor_alignment (SCM grob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, grob, 1);
  return to_scm (calc_joint_anchor_alignment (me));
}

Direction
Break_aligned_interface::calc_joint_anchor_alignment (Grob *me)
{
  // If all elements with non-zero alignment agree in sign, return that
  // direction.  Otherwise, return center.  Just enough thought has been put
  // into this algorithm to serve our immediate needs.
  auto direction = CENTER;

  extract_grob_set (me, "elements", elts);
  for (vsize i = 0; i < elts.size (); i++)
    {
      SCM s = get_property (elts[i], "break-align-anchor-alignment");
      double alignment = from_scm<double> (s, 0.0);
      if (alignment < 0)
        {
          if (direction > CENTER)
            return CENTER; // conflict
          direction = LEFT;
        }
      else if (alignment > 0)
        {
          if (direction < CENTER)
            return CENTER; // conflict
          direction = RIGHT;
        }
    }

  return direction;
}

MAKE_SCHEME_CALLBACK (Break_aligned_interface, calc_extent_aligned_anchor, 1)
SCM
Break_aligned_interface::calc_extent_aligned_anchor (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Real alignment = from_scm<double> (get_property (me, "break-align-anchor-alignment"), 0.0);
  Interval iv = me->extent (me, X_AXIS);

  if (std::isinf (iv[LEFT]) && std::isinf (iv[RIGHT])) /* avoid NaN */
    return to_scm (0.0);

  return to_scm (iv.linear_combination (alignment));
}

MAKE_SCHEME_CALLBACK (Break_aligned_interface, calc_break_visibility, 1)
SCM
Break_aligned_interface::calc_break_visibility (SCM smob)
{
  /* a BreakAlignGroup is break-visible if it has one element that is break-visible */
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  SCM ret = scm_c_make_vector (3, SCM_EOL);
  extract_grob_set (me, "elements", elts);
  for (int dir = 0; dir <= 2; dir++)
    {
      bool visible = false;
      for (vsize i = 0; i < elts.size (); i++)
        {
          SCM vis = get_property (elts[i], "break-visibility");
          if (scm_is_vector (vis) && from_scm<bool> (scm_c_vector_ref (vis, dir)))
            visible = true;
        }
      scm_c_vector_set_x (ret, dir, scm_from_bool (visible));
    }
  return ret;
}

bool Break_aligned_interface::is_non_empty_staff_bar (const Grob *me)
{
  return me
         && scm_is_eq (get_property (me, "break-align-symbol"),
                       ly_symbol2scm ("staff-bar"))
         && !me->extent (me, X_AXIS).is_empty ();
}

ADD_INTERFACE (Break_alignment_interface,
               R"(
The object that performs break alignment.

Three interfaces deal specifically with break alignment:
@enumerate
@item break-alignment-interface (this one),
@item @ref{break-alignable-interface}, and
@item @ref{break-aligned-interface}.
@end enumerate

 Each of these interfaces supports grob properties that use
@w{@emph{break-align symbols}}, which are Scheme symbols that are used to
specify the alignment, ordering, and spacing of certain notational elements
(@q{breakable}@tie{}items).
@subsubheading Available break-align symbols:

@example
ambitus
breathing-sign
clef
cue-clef
cue-end-clef
custos
key-cancellation
key-signature
left-edge
signum-repetitionis
staff-bar
staff-ellipsis
time-signature
@end example
               )",

               /* properties */
               R"(
positioning-done
break-align-orders
               )");

ADD_INTERFACE (Break_alignable_interface,
               R"(
Object that is aligned on a break alignment.
               )",

               /* properties */
               R"(
break-align-symbols
non-break-align-symbols
               )");

ADD_INTERFACE (Break_aligned_interface,
               R"(
Breakable items.
               )",

               /* properties */
               R"(
break-align-anchor
break-align-anchor-alignment
break-align-symbol
space-alist
               )");
