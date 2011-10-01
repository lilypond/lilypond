/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011 Mike Solomon <mike@apollinemike.com>

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

#include <map>
#include <algorithm>

#include "align-interface.hh"
#include "bar-line.hh"
#include "context.hh"
#include "grob.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "span-bar.hh"
#include "engraver.hh"

class Span_bar_stub_engraver : public Engraver
{
  vector<Grob *> spanbars_;
  map<Grob *, Context *> axis_groups_;

public:
  TRANSLATOR_DECLARATIONS (Span_bar_stub_engraver);
protected:
  DECLARE_ACKNOWLEDGER (span_bar);
  DECLARE_ACKNOWLEDGER (hara_kiri_group_spanner);
  void process_acknowledged ();
};

Span_bar_stub_engraver::Span_bar_stub_engraver ()
{
}

void
Span_bar_stub_engraver::acknowledge_span_bar (Grob_info i)
{
  spanbars_.push_back (i.grob ());
}

// note that this can get out of hand if there are lots of vertical axis groups...

void
Span_bar_stub_engraver::acknowledge_hara_kiri_group_spanner (Grob_info i)
{
  axis_groups_[i.grob ()] = i.context ();
}

void
Span_bar_stub_engraver::process_acknowledged ()
{
  if (!spanbars_.size ())
    return;

  Grob *vertical_alignment = Grob::get_root_vertical_alignment ((*axis_groups_.begin ()).first);
  if (!vertical_alignment) // we are at the beginning of a score, so no need for stubs
    return;

  extract_grob_set (vertical_alignment, "elements", elts);

  for (vsize i = 0; i < spanbars_.size (); i++)
    {
      extract_grob_set (spanbars_[i], "elements", bars);
      vector<vsize> bar_axis_indices;
      for (vsize j = 0; j < bars.size (); j++)
        {
          int i = Grob::get_vertical_axis_group_index (bars[j]);
          if (i >= 0)
            bar_axis_indices.push_back ((vsize) i);
        }
      vector<Context *> affected_contexts;
      vector<Grob *> y_parents;
      vector<bool> keep_extent;
      for (vsize j = 0; j < elts.size (); j++)
        {
          if (j > bar_axis_indices[0]
              && j < bar_axis_indices.back ()
              && find (bar_axis_indices.begin (), bar_axis_indices.end (), j) == bar_axis_indices.end ())
            {
              vsize k = 0;
              for (; k < bar_axis_indices.size (); k++)
                if (bar_axis_indices[k] > j)
                  break;

              k--;
              keep_extent.push_back (to_boolean (bars[k]->get_property ("allow-span-bar")));

              Context *c = axis_groups_[elts[j]];
              if (c && c->get_parent_context ())
                {
                  y_parents.push_back (elts[j]);
                  affected_contexts.push_back (c);
                }
            }
        }
      reverse (affected_contexts); // from bottom to top
      reverse (y_parents); // from bottom to top
      reverse (keep_extent); // from bottom to top
      for (vsize j = 0; j < affected_contexts.size (); j++)
        {
          Item *it = new Item (updated_grob_properties (affected_contexts[j], ly_symbol2scm ("SpanBarStub")));
          it->set_parent (spanbars_[i], X_AXIS);
          Grob_info gi = make_grob_info (it, spanbars_[i]->self_scm ());
          gi.rerouting_daddy_context_ = affected_contexts[j];
          announce_grob (gi);
          if (!keep_extent[j])
            it->set_property ("Y-extent", ly_interval2scm (Interval (infinity_f, -infinity_f)));
        }
    }
  spanbars_.clear ();
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Span_bar_stub_engraver, span_bar);
ADD_ACKNOWLEDGER (Span_bar_stub_engraver, hara_kiri_group_spanner);
ADD_TRANSLATOR (Span_bar_stub_engraver,
                /* doc */
                "Make stubs for span bars in all contexts that the span bars cross.",

                /* create */
                "SpanBarStub ",

                /* read */
                "",

                /* write */
                ""
               );
