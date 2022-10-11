/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "system.hh"

#include "align-interface.hh"
#include "all-font-metrics.hh"
#include "axis-group-interface.hh"
#include "break-align-interface.hh"
#include "engraver.hh"
#include "grob-array.hh"
#include "hara-kiri-group-spanner.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "page-layout-problem.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-system.hh"
#include "pointer-group-interface.hh"
#include "protection-pool.hh"
#include "skyline-pair.hh"
#include "staff-symbol-referencer.hh"
#include "system-start-delimiter.hh"
#include "text-interface.hh"
#include "unpure-pure-container.hh"
#include "warn.hh"

#include <algorithm>
#include <limits>
#include <vector>

using std::vector;

Grob_array *
System::all_elements ()
{
  SCM obj = get_object (this, "all-elements");
  return unsmob<Grob_array> (obj);
}

Grob_array const *
System::all_elements () const
{
  SCM obj = get_object (this, "all-elements");
  return unsmob<Grob_array> (obj);
}

System::System (System const &src)
  : Spanner (src)
{
  init_elements ();
}

System::System (SCM s)
  : Spanner (s)
{
  init_elements ();
  interfaces_ = scm_cons (ly_symbol2scm ("system-interface"), interfaces_);
  protection_pool_ = new_protection_pool ();
  protection_pool_add (protection_pool_, self_scm ());
}

void
System::init_elements ()
{
  SCM all = Grob_array::make_array ();
  set_object (this, "all-elements", all);
  unsmob<Grob_array> (all)->set_ordered (false);
}

vsize
System::element_count () const
{
  const Grob_array *a = all_elements ();
  return a ? a->size () : 0;
}

static bool
is_spanner (const Grob *g)
{
  return dynamic_cast<const Spanner *> (g);
}

vsize
System::spanner_count () const
{
  const vector<Grob *> &grobs = all_elements ()->array ();
  return std::count_if (grobs.begin (), grobs.end (), is_spanner);
}

void
System::typeset_grob (Grob *elem)
{
  if (elem->layout_)
    programming_error ("adding element twice");
  else
    {
      elem->layout_ = pscore_->layout ();
      all_elements ()->add (elem);
      if (scm_is_false (elem->protection_pool_))
        elem->protection_pool_ = protection_pool_;
      else
        assert (elem->protection_pool_ == protection_pool_);
      protection_pool_add (protection_pool_, elem->self_scm ());
      elem->unprotect ();
    }
}

void
System::derived_mark () const
{
  if (pscore_)
    scm_gc_mark (pscore_->self_scm ());
  Spanner::derived_mark ();
}

void
System::do_break_substitution_and_fixup_refpoints ()
{
  std::vector<Grob *> &all_elts = all_elements ()->array_reference ();
  for (Grob *g : all_elts)
    g->do_break_processing ();

  /*
    fixups must be done in broken line_of_scores, because new elements
    are put over there.  */
  vsize count = 0;
  for (Grob *child : broken_intos_)
    {
      const std::vector<Grob *> &child_elts
        = static_cast<System *> (child)->all_elements ()->array_reference ();

      for (Grob *g : child_elts)
        g->fixup_refpoint ();

      count += child_elts.size ();
    }

  /*
    needed for doing items.
  */
  for (Grob *g : all_elts)
    g->fixup_refpoint ();

  for (Grob *g : all_elts)
    g->handle_broken_dependencies ();

  handle_broken_dependencies ();

  /* Because the get_property (all-elements) contains items in 3
     versions, handle_broken_dependencies () will leave duplicated
     items in all-elements.  Strictly speaking this is harmless, but
     it leads to duplicated symbols in the output.  uniq makes sure
     that no duplicates are in the list.  */
  for (Grob *child : broken_intos_)
    {
      Grob_array *all_elts_ga = static_cast<System *> (child)->all_elements ();
      all_elts_ga->remove_duplicates ();
      (void) get_property (child, "after-line-breaking");
      for (Grob *g : all_elts_ga->array_reference ())
        {
          (void) get_property (g, "after-line-breaking");
        }
    }

  debug_output (_f ("Element count %zu", count + all_elts.size ()) + "\n");
}

bool
System::accepts_as_bound_item (const Item *) const
{
  return false;
}

bool
System::accepts_as_bound_paper_column (const Paper_column *) const
{
  return true;
}

SCM
System::get_broken_system_grobs ()
{
  SCM ret = SCM_EOL;
  for (vsize i = 0; i < broken_intos_.size (); i++)
    ret = scm_cons (broken_intos_[i]->self_scm (), ret);
  return scm_reverse_x (ret, SCM_EOL);
}

SCM
System::get_paper_systems ()
{
  SCM lines = scm_c_make_vector (broken_intos_.size (), SCM_EOL);
  for (vsize i = 0; i < broken_intos_.size (); i++)
    {
      ::debug_output ("[", false);

      // static_cast is safe because Systems are broken into Systems
      auto *const system = static_cast<System *> (broken_intos_[i]);

      scm_c_vector_set_x (lines, i, system->get_paper_system ());

      ::debug_output (std::to_string (i) + "]", false);
    }
  return lines;
}

vector<Grob *>
System::get_footnote_grobs_in_range (vsize start, vsize end)
{
  vector<Grob *> out;
  extract_grob_set (this, "footnotes-before-line-breaking", footnote_grobs);
  for (vsize i = 0; i < footnote_grobs.size (); i++)
    {
      Grob *at_bat = footnote_grobs[i];
      int pos = at_bat->spanned_column_rank_interval ()[LEFT];
      bool end_of_line_visible = true;
      if (Spanner *s = dynamic_cast<Spanner *> (at_bat))
        {
          Direction spanner_placement
            = from_scm (get_property (s, "spanner-placement"), LEFT);
          if (spanner_placement == CENTER)
            spanner_placement = LEFT;

          pos = s->spanned_column_rank_interval ()[spanner_placement];
          if (Spanner *orig = s->original ())
            {
              at_bat = spanner_placement == LEFT ? orig->broken_intos_[0]
                                                 : orig->broken_intos_.back ();
              pos = at_bat->spanned_column_rank_interval ()[RIGHT];
            }
        }

      if (Item *item = dynamic_cast<Item *> (at_bat))
        {
          /*
            We use this to weed out grobs that fall at the end
            of the line when we want grobs at the beginning.
          */
          end_of_line_visible = item->break_status_dir () == LEFT;

          if (!item->break_visible ())
            continue;
          // safeguard to bring down the column rank so that end of line footnotes show up on the correct line
          if (pos == int (start) && item->break_status_dir () != RIGHT)
            continue;
          if (pos == int (end) && item->break_status_dir () != LEFT)
            continue;
          if (pos != int (end) && pos != int (start)
              && item->break_status_dir () != CENTER)
            continue;
        }

      if (pos < int (start))
        continue;
      if (pos > int (end))
        continue;
      if (pos == int (start) && end_of_line_visible)
        continue;
      if (pos == int (end) && !end_of_line_visible)
        continue;
      if (!at_bat->is_live ())
        continue;
      /*
        TODO
        Sometimes, there are duplicate entries in the all_elements_
        list. In a separate patch, this practice should be squashed
        so that the check below can be eliminated.
      */
      if (find (out.begin (), out.end (), at_bat) != out.end ())
        continue;

      out.push_back (at_bat);
    }
  return out;
}

vector<Real>
System::get_footnote_heights_in_range (vsize start, vsize end)
{
  return internal_get_note_heights_in_range (start, end, true);
}

vector<Real>
System::get_in_note_heights_in_range (vsize start, vsize end)
{
  return internal_get_note_heights_in_range (start, end, false);
}

vector<Real>
System::internal_get_note_heights_in_range (vsize start, vsize end, bool foot)
{
  vector<Grob *> footnote_grobs = get_footnote_grobs_in_range (start, end);
  vector<Real> out;

  for (vsize i = footnote_grobs.size (); i--;)
    if (foot ? !from_scm<bool> (get_property (footnote_grobs[i], "footnote"))
             : from_scm<bool> (get_property (footnote_grobs[i], "footnote")))
      footnote_grobs.erase (footnote_grobs.begin () + i);

  for (vsize i = 0; i < footnote_grobs.size (); i++)
    {
      SCM footnote_markup = get_property (footnote_grobs[i], "footnote-text");

      if (!Text_interface::is_markup (footnote_markup))
        continue;

      SCM props = Lily::layout_extract_page_properties (
        pscore_->layout ()->self_scm ());

      auto stencil = Text_interface::interpret_markup (pscore_->layout (),
                                                       props, footnote_markup);
      out.push_back (stencil.extent (Y_AXIS).length ());
    }

  return out;
}

vsize
System::num_footnotes ()
{
  extract_grob_set (this, "footnotes-after-line-breaking", footnote_grobs);
  return footnote_grobs.size ();
}

bool
grob_2D_less (Grob *g1, Grob *g2)
{
  int sri[] = {0, 0};
  Grob *gs[] = {g1, g2};

  for (int i = 0; i < 2; i++)
    {
      sri[i] = gs[i]->spanned_column_rank_interval ()[LEFT];
      if (Spanner *s = dynamic_cast<Spanner *> (gs[i]))
        {
          if (!s->broken_intos_.empty ())
            {
              auto placement = from_scm<Direction> (
                get_property (s->broken_intos_[0], "spanner-placement"));
              s = (placement == LEFT) ? s->broken_intos_[0]
                                      : s->broken_intos_.back ();
            }
          gs[i] = s;
          if (from_scm<double> (get_property (s, "X-offset"), 0.0) > 0)
            sri[i] = s->spanned_column_rank_interval ()[RIGHT];
        }
    }

  if (sri[0] == sri[1])
    return Grob::vertical_less (gs[0], gs[1]);

  return sri[0] < sri[1];
}

MAKE_SCHEME_CALLBACK (System, footnotes_before_line_breaking,
                      "ly:system::footnotes-before-line-breaking", 1);
SCM
System::footnotes_before_line_breaking (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  vector<Grob *> footnotes;
  SCM grobs_scm = Grob_array::make_array ();
  extract_grob_set (me, "all-elements", elts);
  for (vsize i = 0; i < elts.size (); i++)
    if (elts[i]->internal_has_interface (ly_symbol2scm ("footnote-interface")))
      footnotes.push_back (elts[i]);

  unsmob<Grob_array> (grobs_scm)->set_array (footnotes);
  return grobs_scm;
}

MAKE_SCHEME_CALLBACK (System, footnotes_after_line_breaking,
                      "ly:system::footnotes-after-line-breaking", 1);
SCM
System::footnotes_after_line_breaking (SCM smob)
{
  auto *const sys = LY_ASSERT_SMOB (System, smob, 1);

  Interval_t<int> sri = sys->spanned_column_rank_interval ();
  vector<Grob *> footnote_grobs
    = sys->get_footnote_grobs_in_range (sri[LEFT], sri[RIGHT]);
  std::sort (footnote_grobs.begin (), footnote_grobs.end (), grob_2D_less);

  SCM grobs_scm = Grob_array::make_array ();
  unsmob<Grob_array> (grobs_scm)->set_array (footnote_grobs);
  return grobs_scm;
}

MAKE_SCHEME_CALLBACK (System, vertical_skyline_elements,
                      "ly:system::vertical-skyline-elements", 1);
SCM
System::vertical_skyline_elements (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (System, smob, 1);

  vector<Grob *> vertical_skyline_grobs;
  extract_grob_set (me, "elements", my_elts);
  for (vsize i = 0; i < my_elts.size (); i++)
    if (has_interface<System_start_delimiter> (my_elts[i]))
      vertical_skyline_grobs.push_back (my_elts[i]);

  Grob *align = unsmob<Grob> (get_object (me, "vertical-alignment"));
  if (!align)
    {
      SCM grobs_scm = Grob_array::make_array ();
      unsmob<Grob_array> (grobs_scm)->set_array (vertical_skyline_grobs);
      return grobs_scm;
    }

  extract_grob_set (align, "elements", elts);

  for (vsize i = 0; i < elts.size (); i++)
    if (has_interface<Hara_kiri_group_spanner> (elts[i]))
      vertical_skyline_grobs.push_back (elts[i]);

  SCM grobs_scm = Grob_array::make_array ();
  unsmob<Grob_array> (grobs_scm)->set_array (vertical_skyline_grobs);
  return grobs_scm;
}

void
System::break_into_pieces (vector<Column_x_positions> const &breaking)
{
  for (vsize i = 0; i < breaking.size (); i++)
    {
      System *system = clone ();
      protection_pool_add (protection_pool_, system->self_scm ());
      system->unprotect ();

      // set rank
      system->rank_ = broken_intos_.size ();

      vector<Paper_column *> const &c (breaking[i].cols_);
      pscore_->typeset_system (system);

      int st = c[0]->get_rank ();
      int end = c.back ()->get_rank ();
      Interval iv (pure_y_extent (this, st, end));
      set_property (system, "pure-Y-extent", to_scm (iv));

      system->set_bound (LEFT, c[0]);
      system->set_bound (RIGHT, c.back ());
      SCM system_labels = SCM_EOL;
      for (vsize j = 0; j < c.size (); j++)
        {
          c[j]->translate_axis (breaking[i].config_[j], X_AXIS);
          c[j]->set_system (system);
          /* collect the column labels */
          collect_labels (c[j], &system_labels);
        }
      /*
        Collect labels from any loose columns too: theses will be set on
        an empty bar line or a column which is otherwise unused mid-line
      */
      vector<Paper_column *> const &loose (breaking[i].loose_cols_);
      for (vsize j = 0; j < loose.size (); j++)
        collect_labels (loose[j], &system_labels);

      set_property (system, "labels", system_labels);

      set_loose_columns (system, &breaking[i]);
      broken_intos_.push_back (system);
    }
}

void
System::collect_labels (Grob const *col, SCM *labels)
{
  SCM col_labels = get_property (col, "labels");
  if (scm_is_pair (col_labels))
    *labels = ly_append (col_labels, *labels);
}

void
System::add_column (Paper_column *p)
{
  Grob *me = this;
  Grob_array *ga = unsmob<Grob_array> (get_object (me, "columns"));
  if (!ga)
    {
      SCM scm_ga = Grob_array::make_array ();
      set_object (me, "columns", scm_ga);
      ga = unsmob<Grob_array> (scm_ga);
    }

  p->set_rank (static_cast<int> (ga->size ()));

  ga->add (p);
  Axis_group_interface::add_element (this, p);
}

void
System::pre_processing ()
{
  /*
    Each breakable Item calls back to this System to append two clones of
    itself (for before and after a break) to the vector.  We stop after
    breaking the originals and don't invite the clones to break themselves.
  */
  Grob_array *all = all_elements ();
  vsize num_original_grobs = all->size ();
  for (vsize i = 0; i < num_original_grobs; i++)
    all->grob (i)->break_breakable_item (this);

  debug_output (_f ("Grob count %zu", all->size ()));

  /*
    order is significant: broken grobs are added to the end of the
    array, and should be processed before the original is potentially
    killed.
  */
  for (vsize i = all->size (); i--;)
    all->grob (i)->handle_prebroken_dependencies ();

  for (Grob *g : all->array_reference ())
    g->fixup_refpoint ();

  get_property (this, "before-line-breaking");
  for (Grob *g : all->array_reference ())
    {
      (void) get_property (g, "before-line-breaking");
    }

  get_property (this, "springs-and-rods");
  for (Grob *g : all->array_reference ())
    {
      (void) get_property (g, "springs-and-rods");
    }
}

void
System::post_processing ()
{
  Interval iv (extent (this, Y_AXIS));
  if (iv.is_empty ())
    programming_error ("system with empty extent");
  else
    translate_axis (-iv[UP], Y_AXIS);

  /* Generate all stencils to trigger font loads.
     This might seem inefficient, but Stencils are cached per grob
     anyway. */

  vector<Grob *> all_elts_sorted (all_elements ()->array ());
  uniquify (all_elts_sorted);
  get_stencil ();
  for (Grob *g : all_elts_sorted)
    {
      g->get_stencil ();
    }
}

struct Layer_entry
{
  Grob *grob_;
  int layer_;
};

bool
operator<(Layer_entry const &a, Layer_entry const &b)
{
  return a.layer_ < b.layer_;
}

/* Return the system as a Prob for page layout. The prob holds the
   stencil, but also a reference to the system in 'system-grob
 */
SCM
System::get_paper_system ()
{
  SCM exprs = SCM_EOL;
  SCM *tail = &exprs;

  post_processing ();

  vector<Layer_entry> entries;
  auto &all_elts = all_elements ()->array ();
  for (Grob *g : all_elts)
    {
      Layer_entry e;
      e.grob_ = g;
      e.layer_ = from_scm (get_property (e.grob_, "layer"), 1);

      entries.push_back (e);
    }

  std::sort (entries.begin (), entries.end ());
  for (vsize j = 0; j < entries.size (); j++)
    {
      Grob *g = entries[j].grob_;
      Stencil st = g->get_print_stencil ();

      if (scm_is_null (st.expr ()))
        continue;

      Offset o;
      for (const auto a : {X_AXIS, Y_AXIS})
        o[a] = g->relative_coordinate (this, a);

      Offset extra = from_scm (get_property (g, "extra-offset"), Offset (0, 0))
                     * Staff_symbol_referencer::staff_space (g);

      /* Must copy the stencil, for we cannot change the stencil
         cached in G.  */

      st.translate (o + extra);

      *tail = scm_cons (st.expr (), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  Stencil me = get_print_stencil ();
  if (!scm_is_null (me.expr ()))
    exprs = scm_cons (me.expr (), exprs);

  Interval x (extent (this, X_AXIS));
  Interval y (extent (this, Y_AXIS));
  Stencil sys_stencil (Box (x, y),
                       scm_cons (ly_symbol2scm ("combine-stencil"), exprs));

  Paper_column *left_bound = get_bound (LEFT);
  SCM prop_init = get_property (left_bound, "line-break-system-details");
  Prob *pl = make_paper_system (prop_init);
  paper_system_set_stencil (pl, sys_stencil);

  /* information that the page breaker might need */
  Paper_column *right_bound = get_bound (RIGHT);
  set_property (pl, "vertical-skylines",
                get_property (this, "vertical-skylines"));
  set_property (pl, "page-break-permission",
                get_property (right_bound, "page-break-permission"));
  set_property (pl, "page-turn-permission",
                get_property (right_bound, "page-turn-permission"));
  set_property (pl, "page-break-penalty",
                get_property (right_bound, "page-break-penalty"));
  set_property (pl, "page-turn-penalty",
                get_property (right_bound, "page-turn-penalty"));

  if (right_bound->original () == original ()->get_bound (RIGHT))
    set_property (pl, "last-in-score", SCM_BOOL_T);

  Interval staff_refpoints;
  if (Grob *align = unsmob<Grob> (get_object (this, "vertical-alignment")))
    {
      extract_grob_set (align, "elements", staves);
      for (vsize i = 0; i < staves.size (); i++)
        if (staves[i]->is_live ()
            && Page_layout_problem::is_spaceable (staves[i]))
          staff_refpoints.add_point (
            staves[i]->relative_coordinate (this, Y_AXIS));
    }

  set_property (pl, "staff-refpoint-extent", to_scm (staff_refpoints));
  set_property (pl, "system-grob", self_scm ());

  return pl->unprotect ();
}

vector<Item *>
System::broken_col_range (Item const *left_item, Item const *right_item) const
{
  vector<Item *> ret;

  Paper_column *left_col = left_item->get_column ();
  Paper_column *right_col = right_item->get_column ();

  extract_grob_set (this, "columns", cols);

  vsize end_rank
    = std::min (static_cast<vsize> (right_col->get_rank ()), cols.size ());
  for (vsize i = left_col->get_rank () + 1; i < end_rank; ++i)
    {
      if (Paper_column *c = dynamic_cast<Paper_column *> (cols[i]))
        {
          if (Paper_column::is_breakable (c) && !c->get_system ())
            ret.push_back (c);
        }
    }

  return ret;
}

/**
    Return all columns in the given right-open range, but filter out any unused
    columns, since they might disrupt the spacing problem.
*/
vector<Paper_column *>
System::used_columns_in_range (vsize start, vsize end) const
{
  extract_grob_set (this, "columns", ro_columns);

  vsize last_breakable = ro_columns.size ();

  while (last_breakable--)
    {
      Paper_column *c
        = dynamic_cast<Paper_column *> (ro_columns[last_breakable]);
      if (c && Paper_column::is_breakable (c))
        break;
    }

  end = std::min (end, last_breakable + 1);

  vector<Paper_column *> columns;
  for (vsize i = start; i < end; ++i)
    {
      Paper_column *c = dynamic_cast<Paper_column *> (ro_columns[i]);
      if (c && Paper_column::is_used (c))
        columns.push_back (c);
    }

  return columns;
}

Paper_column *
System::column (vsize which) const
{
  extract_grob_set (this, "columns", columns);
  if (which >= columns.size ())
    return 0;

  return dynamic_cast<Paper_column *> (columns[which]);
}

Paper_score *
System::paper_score () const
{
  return pscore_;
}

System *
get_root_system (Grob *me)
{
  Grob *system_grob = me;

  while (system_grob->get_y_parent ())
    system_grob = system_grob->get_y_parent ();

  return dynamic_cast<System *> (system_grob);
}

MAKE_SCHEME_CALLBACK (System, get_vertical_alignment,
                      "ly:system::get-vertical-alignment", 1);
SCM
System::get_vertical_alignment (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  extract_grob_set (me, "elements", elts);
  Grob *ret = 0;
  for (vsize i = 0; i < elts.size (); i++)
    if (has_interface<Align_interface> (elts[i]))
      {
        if (ret)
          me->programming_error (
            "found multiple vertical alignments in this system");
        ret = elts[i];
      }

  if (!ret)
    {
      me->programming_error ("didn't find a vertical alignment in this system");
      return SCM_EOL;
    }
  return ret->self_scm ();
}

// Finds the neighboring staff in the given direction over bounds
Grob *
System::get_neighboring_staff (Direction dir, Grob *vertical_axis_group,
                               Interval_t<int> bounds)
{
  Grob *align = unsmob<Grob> (get_object (this, "vertical-alignment"));
  if (!align)
    return 0;

  extract_grob_set (align, "elements", elts);
  vsize start = (dir == UP) ? 0 : elts.size () - 1;
  vsize end = (dir == UP) ? elts.size () : VPOS;

  Grob *out = 0;

  for (vsize i = start; i != end; i += dir)
    {
      if (elts[i] == vertical_axis_group)
        return out;

      if (has_interface<Hara_kiri_group_spanner> (elts[i]))
        Hara_kiri_group_spanner::consider_suicide (elts[i]);

      bounds.intersect (elts[i]->spanned_column_rank_interval ());
      if (elts[i]->is_live () && !bounds.is_empty ())
        out = elts[i];
    }

  return 0;
}

Interval
System::pure_refpoint_extent (vsize start, vsize end)
{
  Interval ret;
  Grob *alignment = unsmob<Grob> (get_object (this, "vertical-alignment"));
  if (!alignment)
    return Interval ();

  extract_grob_set (alignment, "elements", staves);
  vector<Real> offsets = Align_interface::get_pure_minimum_translations (
    alignment, staves, Y_AXIS, start, end);

  for (vsize i = 0; i < offsets.size (); ++i)
    if (Page_layout_problem::is_spaceable (staves[i]))
      {
        ret[UP] = offsets[i];
        break;
      }

  for (vsize i = offsets.size (); i--;)
    if (Page_layout_problem::is_spaceable (staves[i]))
      {
        ret[DOWN] = offsets[i];
        break;
      }

  return ret;
}

Interval
System::part_of_line_pure_height (vsize start, vsize end, bool begin)
{
  Grob *alignment = unsmob<Grob> (get_object (this, "vertical-alignment"));
  if (!alignment)
    return Interval ();

  extract_grob_set (alignment, "elements", staves);
  vector<Real> offsets = Align_interface::get_pure_minimum_translations (
    alignment, staves, Y_AXIS, start, end);

  Interval ret;
  for (vsize i = 0; i < staves.size (); ++i)
    {
      Interval iv
        = begin
            ? Axis_group_interface::begin_of_line_pure_height (staves[i], start)
            : Axis_group_interface::rest_of_line_pure_height (staves[i], start,
                                                              end);
      if (i < offsets.size ())
        iv.translate (offsets[i]);
      ret.unite (iv);
    }

  Interval other_elements
    = begin ? Axis_group_interface::begin_of_line_pure_height (this, start)
            : Axis_group_interface::rest_of_line_pure_height (this, start, end);

  ret.unite (other_elements);

  return ret;
}

Interval
System::begin_of_line_pure_height (vsize start, vsize end)
{
  return part_of_line_pure_height (start, end, true);
}

Interval
System::rest_of_line_pure_height (vsize start, vsize end)
{
  return part_of_line_pure_height (start, end, false);
}

// This differs from Axis_group_interface::calc_pure_relevant_grobs
// because here, we are only interested in those few elements that aren't
// descended from VerticalAlignment (ie. things like RehearsalMark, BarLine).
MAKE_SCHEME_CALLBACK (System, calc_pure_relevant_grobs,
                      "ly:system::calc-pure-relevant-grobs", 1);
SCM
System::calc_pure_relevant_grobs (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  extract_grob_set (me, "elements", elts);
  vector<Grob *> relevant_grobs;

  for (vsize i = 0; i < elts.size (); ++i)
    {
      if (!has_interface<Axis_group_interface> (elts[i]))
        {
          if (Item *it = dynamic_cast<Item *> (elts[i]))
            {
              if (it->original ())
                continue;
            }
          relevant_grobs.push_back (elts[i]);
        }
    }

  SCM grobs_scm = Grob_array::make_array ();

  unsmob<Grob_array> (grobs_scm)->set_array (relevant_grobs);
  return grobs_scm;
}

MAKE_SCHEME_CALLBACK (System, height, "ly:system::height", 1);
SCM
System::height (SCM smob)
{
  return Axis_group_interface::height (smob);
}

MAKE_SCHEME_CALLBACK (System, calc_pure_height, "ly:system::calc-pure-height",
                      3);
SCM
System::calc_pure_height (SCM smob, SCM start_scm, SCM end_scm)
{
  System *me = unsmob<System> (smob);
  int start = from_scm<int> (start_scm);
  int end = from_scm<int> (end_scm);

  Interval begin = me->begin_of_line_pure_height (start, end);
  Interval rest = me->rest_of_line_pure_height (start, end);
  begin.unite (rest);

  return to_scm (begin);
}

Paper_column *
System::get_pure_bound (Direction d, vsize start, vsize end)
{
  vector<vsize> const &ranks = pscore_->get_break_ranks ();
  vector<vsize> const &indices = pscore_->get_break_indices ();
  vector<Paper_column *> const &cols = pscore_->get_columns ();

  vsize target_rank = (d == LEFT ? start : end);
  vector<vsize>::const_iterator i = lower_bound (
    ranks.begin (), ranks.end (), target_rank, std::less<vsize> ());

  if (i != ranks.end () && (*i) == target_rank)
    return cols[indices[i - ranks.begin ()]];
  else
    return 0;
}

Paper_column *
System::get_maybe_pure_bound (Direction d, bool pure, vsize start, vsize end)
{
  return pure ? get_pure_bound (d, start, end) : get_bound (d);
}

enum
{
  SPACEABLE_STAVES,
  NONSPACEABLE_STAVES,
  ALL_STAVES
};

static SCM
get_maybe_spaceable_staves (SCM smob, int filter)
{
  System *me = unsmob<System> (smob);
  Grob *align = unsmob<Grob> (get_object (me, "vertical_alignment"));
  SCM ret = SCM_EOL;

  if (align)
    {
      SCM *tail = &ret;
      extract_grob_set (align, "elements", staves);

      for (vsize i = 0; i < staves.size (); ++i)
        {
          bool spaceable = Page_layout_problem::is_spaceable (staves[i]);
          if (staves[i]->is_live ()
              && ((filter == ALL_STAVES)
                  || (filter == SPACEABLE_STAVES && spaceable)
                  || (filter == NONSPACEABLE_STAVES && !spaceable)))
            {
              *tail = scm_cons (staves[i]->self_scm (), SCM_EOL);
              tail = SCM_CDRLOC (*tail);
            }
        }
    }

  return ret;
}

MAKE_SCHEME_CALLBACK (System, get_staves, "ly:system::get-staves", 1)
SCM
System::get_staves (SCM smob)
{
  return get_maybe_spaceable_staves (smob, ALL_STAVES);
}

MAKE_SCHEME_CALLBACK (System, get_spaceable_staves,
                      "ly:system::get-spaceable-staves", 1)
SCM
System::get_spaceable_staves (SCM smob)
{
  return get_maybe_spaceable_staves (smob, SPACEABLE_STAVES);
}

MAKE_SCHEME_CALLBACK (System, get_nonspaceable_staves,
                      "ly:system::get-nonspaceable-staves", 1)
SCM
System::get_nonspaceable_staves (SCM smob)
{
  return get_maybe_spaceable_staves (smob, NONSPACEABLE_STAVES);
}

[[noreturn]] System *
System::make_sticky_same_type (Engraver * /*eng*/, SCM /*type*/, SCM /*cause*/,
                               char const * /*file*/, int /*line*/,
                               char const * /*fun*/)
{
  error (_ ("refusing to create a grob sticking to a System"
            " grob; systems should not be created from custom engravers."));
}

ADD_INTERFACE (System,
               R"(
This is the top-level object: Each object in a score ultimately has a
@code{System} object as its X and Y@tie{}parent.

The @code{system-interface} implies the @iref{spanner-interface}.
               )",

               /* properties */
               R"(
all-elements
columns
footnote-stencil
footnotes-before-line-breaking
footnotes-after-line-breaking
in-note-direction
in-note-padding
in-note-stencil
labels
page-number
pure-Y-extent
rank-on-page
vertical-alignment
               )");
