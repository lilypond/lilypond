/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "engraver.hh"
#include "protected-scm.hh"
#include "break-align-interface.hh"
#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "context.hh"
#include "translator-group.hh"
#include "item.hh"

#include "translator.icc"

class Break_align_engraver : public Engraver
{
  Item *align_ = nullptr;
  SCM column_alist_ = SCM_EOL;
  Item *left_edge_ = nullptr;

  void add_to_group (SCM, Item *);
  void create_alignment ();

protected:
  void stop_translation_timestep ();
  void derived_mark () const override;

public:
  TRANSLATOR_DECLARATIONS (Break_align_engraver);
  void acknowledge_break_aligned (Grob_info_t<Item>);
  void acknowledge_break_alignable (Grob_info_t<Item>);
};

void
Break_align_engraver::stop_translation_timestep ()
{
  column_alist_ = SCM_EOL;

  align_ = nullptr;
  left_edge_ = nullptr;
}

Break_align_engraver::Break_align_engraver (Context *c)
  : Engraver (c)
{
}

void
Break_align_engraver::derived_mark () const
{
  scm_gc_mark (column_alist_);
}

void
Break_align_engraver::acknowledge_break_alignable (Grob_info_t<Item> inf)
{
  auto *const item = inf.grob ();

  if (item->get_x_parent ())
    return;

  // Handling musical items is more involved because they might need to be
  // aligned with notation (note heads, etc.).  We currently leave that to
  // other engravers, but maybe it could be done here.
  if (!Item::is_non_musical (item))
    return;

  if (!align_)
    create_alignment ();

  item->set_x_parent (align_);
}

// Clef, BarLine, etc. are break-aligned grobs
void
Break_align_engraver::acknowledge_break_aligned (Grob_info_t<Item> inf)
{
  auto *const item = inf.grob ();

  /*
    Removed check for item->empty (X_AXIS). --hwn 20/1/04
  */
  if (item->get_x_parent ())
    return;

  if (!Item::is_non_musical (item))
    return;

  SCM align_name = get_property (item, "break-align-symbol");
  if (!scm_is_symbol (align_name))
    return;

  create_alignment ();

  // Create a single LeftEdge that appears to come from the same engraver
  // as the first staff-resident, break-aligned grob that we see.  This is
  // questionable and may contribute to problems discussed in issue #5385.
  // Practically, this is probably fine for single-staff scores.
  //
  // Break-aligned grobs can originate outside of a Staff context, but we
  // don't want to create a LeftEdge then (issue #6134).  The createSpacing
  // property tells us whether the grob originated within a Staff or
  // similar context.
  if (!left_edge_)
    {
      auto *const eng = inf.origin_engraver ();
      if (from_scm<bool> (get_property (eng, "createSpacing")))
        {
          left_edge_ = eng->make_item ("LeftEdge", SCM_EOL);
          add_to_group (get_property (left_edge_, "break-align-symbol"),
                        left_edge_);
        }
    }

  add_to_group (align_name, item);
}

void
Break_align_engraver::create_alignment ()
{
  if (!align_)
    align_ = make_item ("BreakAlignment", SCM_EOL);
}

void
Break_align_engraver::add_to_group (SCM align_name, Item *item)
{
  SCM s = ly_assoc (align_name, column_alist_);
  Item *group = nullptr;

  if (scm_is_true (s))
    {
      group = unsmob<Item> (scm_cdr (s));
    }
  else
    {
      group = make_item ("BreakAlignGroup", item->self_scm ());

      set_property (group, "break-align-symbol", align_name);
      group->set_y_parent (align_);

      column_alist_
        = scm_assoc_set_x (column_alist_, align_name, group->self_scm ());

      Break_alignment_interface::add_element (align_, group);
    }
  Axis_group_interface::add_element (group, item);
}

void
Break_align_engraver::boot ()
{
  ADD_ACKNOWLEDGER (break_aligned);
  ADD_ACKNOWLEDGER (break_alignable);
}

ADD_TRANSLATOR (Break_align_engraver,
                /* doc */
                R"(
Align grobs with corresponding @code{break-align-symbols} into groups, and
order the groups according to @code{breakAlignOrder}.  The left edge of the
alignment gets a separate group, with a symbol @code{left-edge}.
                )",

                /* create */
                R"(
BreakAlignment
BreakAlignGroup
LeftEdge
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
