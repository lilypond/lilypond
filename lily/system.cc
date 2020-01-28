/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <algorithm>
#include <limits>

#include "system.hh"

#include "align-interface.hh"
#include "all-font-metrics.hh"
#include "axis-group-interface.hh"
#include "break-align-interface.hh"
#include "grob-array.hh"
#include "hara-kiri-group-spanner.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "lookup.hh"
#include "main.hh"
#include "output-def.hh"
#include "page-layout-problem.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-system.hh"
#include "pointer-group-interface.hh"
#include "skyline-pair.hh"
#include "staff-symbol-referencer.hh"
#include "system-start-delimiter.hh"
#include "text-interface.hh"
#include "unpure-pure-container.hh"
#include "warn.hh"

using std::vector;

System::System (System const &src) : Spanner (src)
{
  all_elements_ = 0;
  pscore_ = 0;
  rank_ = 0;
  init_elements ();
}

System::System (SCM s) : Spanner (s)
{
  all_elements_ = 0;
  rank_ = 0;
  init_elements ();
}

void
System::init_elements ()
{
  SCM scm_arr = Grob_array::make_array ();
  all_elements_ = unsmob<Grob_array> (scm_arr);
  all_elements_->set_ordered (false);
  set_object ("all-elements", scm_arr);
}

vsize
System::element_count () const
{
  return all_elements_->size ();
}

static bool
is_spanner (const Grob *g)
{
  return dynamic_cast<const Spanner *> (g);
}

vsize
System::spanner_count () const
{
  const vector<Grob *> &grobs = all_elements_->array ();
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
      all_elements_->add (elem);
      elem->unprotect ();
    }
}

void
System::derived_mark () const
{
  const vector<Grob *> &arr = all_elements_->array ();
  for (vsize i = arr.size (); i--;)
    scm_gc_mark (arr[i]->self_scm ());

  if (pscore_)
    scm_gc_mark (pscore_->self_scm ());

  Spanner::derived_mark ();
}

static void
fixup_refpoints (vector<Grob *> const &grobs)
{
  for (vsize i = grobs.size (); i--;)
    grobs[i]->fixup_refpoint ();
}

void
System::do_break_substitution_and_fixup_refpoints ()
{
  for (vsize i = 0; i < all_elements_->size (); i++)
    {
      Grob *g = all_elements_->grob (i);
      if (g->internal_has_interface (ly_symbol2scm ("only-prebreak-interface")))
        {
          /*
            Kill no longer needed grobs.
          */
          Item *it = dynamic_cast<Item *> (g);
          if (it && Item::is_non_musical (it))
            {
              it->find_prebroken_piece (LEFT)->suicide ();
              it->find_prebroken_piece (RIGHT)->suicide ();
            }
          g->suicide ();
        }
      else if (g->is_live ())
        g->do_break_processing ();
    }

  /*
    fixups must be done in broken line_of_scores, because new elements
    are put over there.  */
  vsize count = 0;
  for (vsize i = 0; i < broken_intos_.size (); i++)
    {
      Grob *se = broken_intos_[i];

      extract_grob_set (se, "all-elements", all_elts);
      for (vsize j = 0; j < all_elts.size (); j++)
        {
          Grob *g = all_elts[j];
          g->fixup_refpoint ();
        }

      count += all_elts.size ();
    }

  /*
    needed for doing items.
  */
  fixup_refpoints (all_elements_->array ());

  for (vsize i = 0; i < all_elements_->size (); i++)
    all_elements_->grob (i)->handle_broken_dependencies ();

  handle_broken_dependencies ();

  /* Because the get_property (all-elements) contains items in 3
     versions, handle_broken_dependencies () will leave duplicated
     items in all-elements.  Strictly speaking this is harmless, but
     it leads to duplicated symbols in the output.  uniq makes sure
     that no duplicates are in the list.  */
  for (vsize i = 0; i < broken_intos_.size (); i++)
    {
      System *child = dynamic_cast<System *> (broken_intos_[i]);
      child->all_elements_->remove_duplicates ();

      for (vsize j = 0; j < child->all_elements_->size (); j++)
        {
          Grob *g = child->all_elements_->grob (j);

          (void)g->get_property ("after-line-breaking");
        }
    }

  debug_output (_f ("Element count %zu", count + element_count ()) + "\n");
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
      debug_output ("[", false);

      System *system = dynamic_cast<System *> (broken_intos_[i]);

      scm_c_vector_set_x (lines, i, system->get_paper_system ());

      debug_output (std::to_string (i) + "]", false);
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
      int pos = at_bat->spanned_rank_interval ()[LEFT];
      bool end_of_line_visible = true;
      if (Spanner *s = dynamic_cast<Spanner *> (at_bat))
        {
          Direction spanner_placement
              = robust_scm2dir (s->get_property ("spanner-placement"), LEFT);
          if (spanner_placement == CENTER)
            spanner_placement = LEFT;

          pos = s->spanned_rank_interval ()[spanner_placement];
          if (Spanner *orig = s->original ())
            {
              at_bat = spanner_placement == LEFT ? orig->broken_intos_[0]
                                                 : orig->broken_intos_.back ();
              pos = at_bat->spanned_rank_interval ()[RIGHT];
            }
        }

      if (Item *item = dynamic_cast<Item *> (at_bat))
        {
          /*
            We use this to weed out grobs that fall at the end
            of the line when we want grobs at the beginning.
          */
          end_of_line_visible = item->break_status_dir () == LEFT;

          if (!Item::break_visible (item))
            continue;
          // safeguard to bring down the column rank so that end of line
          // footnotes show up on the correct line
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
    if (foot ? !to_boolean (footnote_grobs[i]->get_property ("footnote"))
             : to_boolean (footnote_grobs[i]->get_property ("footnote")))
      footnote_grobs.erase (footnote_grobs.begin () + i);

  for (vsize i = 0; i < footnote_grobs.size (); i++)
    {
      SCM footnote_markup = footnote_grobs[i]->get_property ("footnote-text");

      if (!Text_interface::is_markup (footnote_markup))
        continue;

      SCM props = Lily::layout_extract_page_properties (
          pscore_->layout ()->self_scm ());

      SCM footnote_stl = Text_interface::interpret_markup (
          pscore_->layout ()->self_scm (), props, footnote_markup);

      Stencil *footnote_stencil = unsmob<Stencil> (footnote_stl);
      out.push_back (footnote_stencil->extent (Y_AXIS).length ());
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
      sri[i] = gs[i]->spanned_rank_interval ()[LEFT];
      if (Spanner *s = dynamic_cast<Spanner *> (gs[i]))
        {
          if (s->broken_intos_.size ())
            s = (scm_to_int (
                     s->broken_intos_[0]->get_property ("spanner-placement"))
                         == LEFT
                     ? s->broken_intos_[0]
                     : s->broken_intos_.back ());
          gs[i] = s;
          if (robust_scm2double (s->get_property ("X-offset"), 0.0) > 0)
            sri[i] = s->spanned_rank_interval ()[RIGHT];
        }
    }

  if (sri[0] == sri[1])
    return Grob::vertical_less (gs[0], gs[1]);

  return sri[0] < sri[1];
}

MAKE_SCHEME_CALLBACK (System, footnotes_before_line_breaking, 1);
SCM
System::footnotes_before_line_breaking (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  vector<Grob *> footnotes;
  SCM grobs_scm = Grob_array::make_array ();
  extract_grob_set (me, "all-elements", elts);
  for (vsize i = 0; i < elts.size (); i++)
    if (elts[i]->internal_has_interface (ly_symbol2scm ("footnote-interface")))
      footnotes.push_back (elts[i]);

  unsmob<Grob_array> (grobs_scm)->set_array (footnotes);
  return grobs_scm;
}

MAKE_SCHEME_CALLBACK (System, footnotes_after_line_breaking, 1);
SCM
System::footnotes_after_line_breaking (SCM smob)
{
  Spanner *sys_span = unsmob<Spanner> (smob);
  System *sys = dynamic_cast<System *> (sys_span);
  Interval_t<int> sri = sys->spanned_rank_interval ();
  vector<Grob *> footnote_grobs
      = sys->get_footnote_grobs_in_range (sri[LEFT], sri[RIGHT]);
  vector_sort (footnote_grobs, grob_2D_less);

  SCM grobs_scm = Grob_array::make_array ();
  unsmob<Grob_array> (grobs_scm)->set_array (footnote_grobs);
  return grobs_scm;
}

MAKE_SCHEME_CALLBACK (System, vertical_skyline_elements, 1);
SCM
System::vertical_skyline_elements (SCM smob)
{
  Grob *me_grob = unsmob<Grob> (smob);
  vector<Grob *> vertical_skyline_grobs;
  extract_grob_set (me_grob, "elements", my_elts);
  for (vsize i = 0; i < my_elts.size (); i++)
    if (has_interface<System_start_delimiter> (my_elts[i]))
      vertical_skyline_grobs.push_back (my_elts[i]);

  System *me = dynamic_cast<System *> (me_grob);
  Grob *align = unsmob<Grob> (me->get_object ("vertical-alignment"));
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

      // set rank
      {
        vsize rank = broken_intos_.size ();
        if (rank >= std::numeric_limits<rank_type>::max ())
          programming_error ("too many systems");
        system->rank_ = static_cast<rank_type> (rank);
      }

      vector<Paper_column *> const &c (breaking[i].cols_);
      pscore_->typeset_system (system);

      int st = c[0]->get_rank ();
      int end = c.back ()->get_rank ();
      Interval iv (pure_y_extent (this, st, end));
      system->set_property ("pure-Y-extent", ly_interval2scm (iv));

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

      system->set_property ("labels", system_labels);

      set_loose_columns (system, &breaking[i]);
      broken_intos_.push_back (system);
    }
}

void
System::collect_labels (Grob const *col, SCM *labels)
{
  SCM col_labels = col->get_property ("labels");
  if (scm_is_pair (col_labels))
    *labels = scm_append (scm_list_2 (col_labels, *labels));
}

void
System::add_column (Paper_column *p)
{
  Grob *me = this;
  Grob_array *ga = unsmob<Grob_array> (me->get_object ("columns"));
  if (!ga)
    {
      SCM scm_ga = Grob_array::make_array ();
      me->set_object ("columns", scm_ga);
      ga = unsmob<Grob_array> (scm_ga);
    }

  p->set_rank (ga->size ());

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
  vsize num_original_grobs = all_elements_->size ();
  for (vsize i = 0; i < num_original_grobs; i++)
    all_elements_->grob (i)->break_breakable_item (this);

  debug_output (_f ("Grob count %zu", element_count ()));

  /*
    order is significant: broken grobs are added to the end of the
    array, and should be processed before the original is potentially
    killed.
  */
  for (vsize i = all_elements_->size (); i--;)
    all_elements_->grob (i)->handle_prebroken_dependencies ();

  fixup_refpoints (all_elements_->array ());

  for (vsize i = 0; i < all_elements_->size (); i++)
    {
      Grob *g = all_elements_->grob (i);
      (void)g->get_property ("before-line-breaking");
    }

  for (vsize i = 0; i < all_elements_->size (); i++)
    {
      Grob *e = all_elements_->grob (i);
      (void)e->get_property ("springs-and-rods");
    }
}

void
System::post_processing ()
{
  Interval iv (extent (this, Y_AXIS));
  if (iv.is_empty ())
    programming_error ("system with empty extent");
  else
    translate_axis (-iv[MAX], Y_AXIS);

  /* Generate all stencils to trigger font loads.
     This might seem inefficient, but Stencils are cached per grob
     anyway. */

  vector<Grob *> all_elts_sorted (all_elements_->array ());
  uniquify (all_elts_sorted);
  get_stencil ();
  for (vsize i = all_elts_sorted.size (); i--;)
    {
      Grob *g = all_elts_sorted[i];
      g->get_stencil ();
    }
}

struct Layer_entry
{
  Grob *grob_;
  int layer_;
};

bool
operator< (Layer_entry const &a, Layer_entry const &b)
{
  return a.layer_ < b.layer_;
}

SCM
System::get_paper_system ()
{
  SCM exprs = SCM_EOL;
  SCM *tail = &exprs;

  post_processing ();

  vector<Layer_entry> entries;
  for (vsize j = 0; j < all_elements_->size (); j++)
    {
      Layer_entry e;
      e.grob_ = all_elements_->grob (j);
      e.layer_ = robust_scm2int (e.grob_->get_property ("layer"), 1);

      entries.push_back (e);
    }

  vector_sort (entries, std::less<Layer_entry> ());
  for (vsize j = 0; j < entries.size (); j++)
    {
      Grob *g = entries[j].grob_;
      Stencil st = g->get_print_stencil ();

      if (scm_is_null (st.expr ()))
        continue;

      Offset o;
      for (int a = X_AXIS; a < NO_AXES; a++)
        o[Axis (a)] = g->relative_coordinate (this, Axis (a));

      Offset extra
          = robust_scm2offset (g->get_property ("extra-offset"), Offset (0, 0))
            * Staff_symbol_referencer::staff_space (g);

      /* Must copy the stencil, for we cannot change the stencil
         cached in G.  */

      st.translate (o + extra);

      *tail = scm_cons (st.expr (), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  if (Stencil *me = get_stencil ())
    exprs = scm_cons (me->expr (), exprs);

  Interval x (extent (this, X_AXIS));
  Interval y (extent (this, Y_AXIS));
  Stencil sys_stencil (Box (x, y),
                       scm_cons (ly_symbol2scm ("combine-stencil"), exprs));
  if (debug_skylines)
    {
      Skyline_pair *skylines
          = unsmob<Skyline_pair> (get_property ("vertical-skylines"));
      if (skylines)
        {
          Stencil up = Lookup::points_to_line_stencil (
              0.1, (*skylines)[UP].to_points (X_AXIS));
          Stencil down = Lookup::points_to_line_stencil (
              0.1, (*skylines)[DOWN].to_points (X_AXIS));
          sys_stencil.add_stencil (up.in_color (1.0, 0.0, 0.0));
          sys_stencil.add_stencil (down.in_color (0.0, 1.0, 0.0));
        }
    }

  Paper_column *left_bound = get_bound (LEFT);
  SCM prop_init = left_bound->get_property ("line-break-system-details");
  Prob *pl = make_paper_system (prop_init);
  paper_system_set_stencil (pl, sys_stencil);

  /* information that the page breaker might need */
  Paper_column *right_bound = get_bound (RIGHT);
  pl->set_property ("vertical-skylines", get_property ("vertical-skylines"));
  pl->set_property ("page-break-permission",
                    right_bound->get_property ("page-break-permission"));
  pl->set_property ("page-turn-permission",
                    right_bound->get_property ("page-turn-permission"));
  pl->set_property ("page-break-penalty",
                    right_bound->get_property ("page-break-penalty"));
  pl->set_property ("page-turn-penalty",
                    right_bound->get_property ("page-turn-penalty"));

  if (right_bound->original () == original ()->get_bound (RIGHT))
    pl->set_property ("last-in-score", SCM_BOOL_T);

  Interval staff_refpoints;
  if (Grob *align = unsmob<Grob> (get_object ("vertical-alignment")))
    {
      extract_grob_set (align, "elements", staves);
      for (vsize i = 0; i < staves.size (); i++)
        if (staves[i]->is_live ()
            && Page_layout_problem::is_spaceable (staves[i]))
          staff_refpoints.add_point (
              staves[i]->relative_coordinate (this, Y_AXIS));
    }

  pl->set_property ("staff-refpoint-extent", ly_interval2scm (staff_refpoints));
  pl->set_property ("system-grob", self_scm ());

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

  while (system_grob->get_parent (Y_AXIS))
    system_grob = system_grob->get_parent (Y_AXIS);

  return dynamic_cast<System *> (system_grob);
}

MAKE_SCHEME_CALLBACK (System, get_vertical_alignment, 1);
SCM
System::get_vertical_alignment (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
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
  Grob *align = unsmob<Grob> (get_object ("vertical-alignment"));
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

      bounds.intersect (elts[i]->spanned_rank_interval ());
      if (elts[i]->is_live () && !bounds.is_empty ())
        out = elts[i];
    }

  return 0;
}

Interval
System::pure_refpoint_extent (vsize start, vsize end)
{
  Interval ret;
  Grob *alignment = unsmob<Grob> (get_object ("vertical-alignment"));
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
  Grob *alignment = unsmob<Grob> (get_object ("vertical-alignment"));
  if (!alignment)
    return Interval ();

  extract_grob_set (alignment, "elements", staves);
  vector<Real> offsets = Align_interface::get_pure_minimum_translations (
      alignment, staves, Y_AXIS, start, end);

  Interval ret;
  for (vsize i = 0; i < staves.size (); ++i)
    {
      Interval iv = begin ? Axis_group_interface::begin_of_line_pure_height (
                        staves[i], start)
                          : Axis_group_interface::rest_of_line_pure_height (
                              staves[i], start, end);
      if (i < offsets.size ())
        iv.translate (offsets[i]);
      ret.unite (iv);
    }

  Interval other_elements
      = begin
            ? Axis_group_interface::begin_of_line_pure_height (this, start)
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
MAKE_SCHEME_CALLBACK (System, calc_pure_relevant_grobs, 1);
SCM
System::calc_pure_relevant_grobs (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  extract_grob_set (me, "elements", elts);
  vector<Grob *> relevant_grobs;

  for (vsize i = 0; i < elts.size (); ++i)
    {
      if (!has_interface<Axis_group_interface> (elts[i]))
        {
          relevant_grobs.push_back (elts[i]);

          if (Item *it = dynamic_cast<Item *> (elts[i]))
            {
              for (LEFT_and_RIGHT (d))
                {
                  Item *piece = it->find_prebroken_piece (d);
                  if (piece && piece->is_live ())
                    relevant_grobs.push_back (piece);
                }
            }
        }
    }

  SCM grobs_scm = Grob_array::make_array ();

  unsmob<Grob_array> (grobs_scm)->set_array (relevant_grobs);
  return grobs_scm;
}

MAKE_SCHEME_CALLBACK (System, height, 1);
SCM
System::height (SCM smob)
{
  return Axis_group_interface::height (smob);
}

MAKE_SCHEME_CALLBACK (System, calc_pure_height, 3);
SCM
System::calc_pure_height (SCM smob, SCM start_scm, SCM end_scm)
{
  System *me = unsmob<System> (smob);
  int start = scm_to_int (start_scm);
  int end = scm_to_int (end_scm);

  Interval begin = me->begin_of_line_pure_height (start, end);
  Interval rest = me->rest_of_line_pure_height (start, end);
  begin.unite (rest);

  return ly_interval2scm (begin);
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
  Grob *align = unsmob<Grob> (me->get_object ("vertical_alignment"));
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

MAKE_SCHEME_CALLBACK (System, get_staves, 1)
SCM
System::get_staves (SCM smob)
{
  return get_maybe_spaceable_staves (smob, ALL_STAVES);
}

MAKE_SCHEME_CALLBACK (System, get_spaceable_staves, 1)
SCM
System::get_spaceable_staves (SCM smob)
{
  return get_maybe_spaceable_staves (smob, SPACEABLE_STAVES);
}

MAKE_SCHEME_CALLBACK (System, get_nonspaceable_staves, 1)
SCM
System::get_nonspaceable_staves (SCM smob)
{
  return get_maybe_spaceable_staves (smob, NONSPACEABLE_STAVES);
}

ADD_INTERFACE (System,
               "This is the top-level object: Each object in a score"
               " ultimately has a @code{System} object as its X and"
               " Y@tie{}parent.",

               /* properties */
               "all-elements "
               "columns "
               "footnote-stencil "
               "footnotes-before-line-breaking "
               "footnotes-after-line-breaking "
               "in-note-direction "
               "in-note-padding "
               "in-note-stencil "
               "labels "
               "pure-Y-extent "
               "vertical-alignment ");
