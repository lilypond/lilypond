/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2022 Mike Solomon <mike@mikesolomon.org>

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
#include "pointer-group-interface.hh"
#include "engraver.hh"

#include "translator.icc"

#include <algorithm>

using std::vector;

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

class Span_bar_stub_engraver : public Engraver
{
  vector<Grob *> spanbars_;
  SCM axis_groups_;

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
  axis_groups_ = SCM_EOL;
}

void
Span_bar_stub_engraver::derived_mark () const
{
  scm_gc_mark (axis_groups_);
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
  axis_groups_ = scm_cons (s, axis_groups_);
}

void
Span_bar_stub_engraver::process_acknowledged ()
{
  if (!spanbars_.size ())
    return;

  if (!scm_is_pair (axis_groups_))
    {
      programming_error ("At least one vertical axis group needs to be created "
                         "in the first time step.");
      return;
    }
  Grob *vertical_alignment = Grob::get_root_vertical_alignment (
    unsmob<Grob> (scm_caar (axis_groups_)));
  if (
    !vertical_alignment) // we are at the beginning of a score, so no need for stubs
    return;

  for (vsize i = 0; i < spanbars_.size (); i++)
    {
      extract_grob_set (spanbars_[i], "elements", bars);
      vector<vsize> bar_axis_indices;
      for (vsize j = 0; j < bars.size (); j++)
        {
          int i = Grob::get_vertical_axis_group_index (bars[j]);
          if (i >= 0)
            bar_axis_indices.push_back (static_cast<vsize> (i));
        }
      vector<Context *> affected_contexts;
      vector<Grob *> y_parents;
      vector<bool> keep_extent;
      for (SCM s = axis_groups_; scm_is_pair (s); s = scm_cdr (s))
        {
          Context *c = unsmob<Context> (scm_cdar (s));
          Grob *g = unsmob<Grob> (scm_caar (s));
          if (!c || !g)
            continue;
          if (c->is_removable ())
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
          Item *it
            = new Item (Grob_property_info (affected_contexts[j],
                                            ly_symbol2scm ("SpanBarStub"))
                          .updated ());
          it->set_x_parent (spanbars_[i]);
          Grob_info gi = make_grob_info (it, spanbars_[i]->self_scm ());
          announce_grob (gi, affected_contexts[j]);
          if (!keep_extent[j])
            it->suicide ();
        }
    }

  spanbars_.clear ();
}

// removes all unused contexts
void
Span_bar_stub_engraver::stop_translation_timestep ()
{
  SCM axis_groups = SCM_EOL;
  for (SCM s = axis_groups_; scm_is_pair (s); s = scm_cdr (s))
    {
      Context *c = unsmob<Context> (scm_cdar (s));
      Grob *g = unsmob<Grob> (scm_caar (s));
      if (!c || !g)
        continue;
      if (c->is_removable ())
        continue;
      axis_groups = scm_cons (scm_car (s), axis_groups);
    }
  axis_groups_ = axis_groups;
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
