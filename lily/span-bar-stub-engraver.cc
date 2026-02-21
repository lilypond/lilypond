/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2023 Mike Solomon <mike@mikesolomon.org>

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

#include "align-interface.hh"
#include "context.hh"
#include "grob.hh"
#include "grob-properties.hh"
#include "item.hh"
#include "ly-scm-list.hh"
#include "pointer-group-interface.hh"
#include "engraver.hh"

#include "translator.icc"

#include <algorithm>

/*
  The Span_bar_stub_engraver creates SpanBarStub grobs in the contexts
  that a grouping context contains.  For example, if a PianoStaff contains
  two Staffs, a Dynamics, and a Lyrics, SpanBarStubs will be created in
  all contexts that do not have bar lines (Dynamics and Lyrics).

  We only want to create these SpanBarStubs in contexts that the SpanBar
  traverses.  However, Contexts do not contain layout information and it
  is thus difficult to know if they will eventually be above or below other
  Contexts.  To determine this we use the VerticalAxisGroup created in the
  Context.  We relate VerticalAxisGroups to Contexts in the variable
  axis_groups_ and weed out unused contexts after each translation timestep.

  Note that SpanBarStubs exist for pure height calculations ONLY.
  They should never be visually present on the page and should never
  be engraved in contexts where BarLines are engraved.
*/

class Span_bar_stub_engraver final : public Engraver
{
  std::vector<Grob *> spanbars_;
  ly_scm_list axis_groups_;

public:
  TRANSLATOR_DECLARATIONS (Span_bar_stub_engraver);

protected:
  void acknowledge_span_bar (Grob_info);
  void acknowledge_hara_kiri_group_spanner (Grob_info);
  void process_acknowledged ();
  void stop_translation_timestep ();
  void derived_mark () const override;
};

Span_bar_stub_engraver::Span_bar_stub_engraver (Context *c)
  : Engraver (c)
{
}

void
Span_bar_stub_engraver::derived_mark () const
{
  axis_groups_.gc_mark ();
}

void
Span_bar_stub_engraver::acknowledge_span_bar (Grob_info i)
{
  spanbars_.push_back (i.grob ());
}

void
Span_bar_stub_engraver::acknowledge_hara_kiri_group_spanner (Grob_info i)
{
  SCM s = scm_cons (i.grob ()->self_scm (),
                    i.origin_engraver ()->context ()->self_scm ());
  axis_groups_.insert_before (axis_groups_.begin (), s);
}

void
Span_bar_stub_engraver::process_acknowledged ()
{
  if (!spanbars_.size ())
    return;

  if (axis_groups_.empty ())
    {
      programming_error (
        "At least one vertical axis group needs to be created "
        "in the first time step.");
      return;
    }
  Grob *vertical_alignment = Grob::get_root_vertical_alignment (
    unsmob<Grob> (scm_car (*axis_groups_.begin ())));
  if (
    !vertical_alignment) // we are at the beginning of a score, so no need for stubs
    return;

  for (auto *const spanbar : spanbars_)
    {
      extract_grob_set (spanbar, "elements", bars);
      std::vector<vsize> bar_axis_indices;
      for (const auto &bar : bars)
        {
          int i = Grob::get_vertical_axis_group_index (bar);
          if (i >= 0)
            bar_axis_indices.push_back (static_cast<vsize> (i));
        }
      std::vector<Context *> affected_contexts;
      std::vector<Grob *> y_parents;
      std::vector<bool> keep_extent;
      for (SCM s : axis_groups_)
        {
          auto *const c = unsmob<Context> (scm_cdr (s));
          if (!c || c->is_removable ())
            continue;

          auto *const g = unsmob<Grob> (scm_car (s));
          if (!g)
            continue;

          vsize j = Grob::get_vertical_axis_group_index (g);
          if (j > bar_axis_indices[0] && j < bar_axis_indices.back ()
              && find (bar_axis_indices.begin (), bar_axis_indices.end (), j)
                   == bar_axis_indices.end ())
            {
              vsize k = 0;
              for (; k < bar_axis_indices.size (); k++)
                if (bar_axis_indices[k] > j)
                  break;

              k--;

              if (c && c->get_parent ())
                {
                  keep_extent.push_back (
                    from_scm<bool> (get_property (bars[k], "allow-span-bar")));
                  y_parents.push_back (g);
                  affected_contexts.push_back (c);
                }
            }
        }

      for (vsize j = 0; j < affected_contexts.size (); j++)
        {
          auto *const ctx = affected_contexts[j];
          auto *const it
            = new Item (Grob_property_info (ctx, ly_symbol2scm ("SpanBarStub"))
                          .updated ());
          it->set_x_parent (spanbar);
          auto gi = make_grob_info (it, spanbar->self_scm ());
          announce_grob (gi, ctx);
          if (!keep_extent[j])
            it->suicide ();
        }
    }

  spanbars_.clear ();
}

void
Span_bar_stub_engraver::stop_translation_timestep ()
{
  // remove unused contexts
  axis_groups_.remove_if ([] (SCM s) {
    auto *const c = unsmob<Context> (scm_cdr (s));
    if (!c || c->is_removable ())
      return true;

    if (!unsmob<Grob> (scm_car (s)))
      return true;

    return false;
  });
}

void
Span_bar_stub_engraver::boot ()
{
  ADD_ACKNOWLEDGER (span_bar);
  ADD_ACKNOWLEDGER (hara_kiri_group_spanner);
}

ADD_TRANSLATOR (Span_bar_stub_engraver,
                /* doc */
                R"(
Make stubs for span bars in all contexts that the span bars cross.
                )",

                /* create */
                R"(
SpanBarStub
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
