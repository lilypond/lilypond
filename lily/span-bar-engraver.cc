/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "item.hh"
#include "engraver.hh"
#include "pointer-group-interface.hh"
#include "lily-imports.hh"

#include "translator.icc"

#include <algorithm>
#include <vector>

/**

Make bars that span multiple "staves". Catch bars, and span a
Span_bar over them if we find more than 2 bars.  Vertical alignment
of staves changes the appearance of spanbars.  It is up to the
aligner (Vertical_align_engraver, in this case, to add extra
dependencies to the spanbars.
*/
class Span_bar_engraver final : public Engraver
{
  Item *spanbar_ = nullptr;
  bool make_spanbar_ = false;
  std::vector<Item *> bars_;
  std::vector<Item *> stubs_;

public:
  TRANSLATOR_DECLARATIONS (Span_bar_engraver);

protected:
  void acknowledge_bar_line (Grob_info_t<Item>);
  void acknowledge_span_bar_stub (Grob_info_t<Item>);
  void stop_translation_timestep ();
  void process_acknowledged ();
};

Span_bar_engraver::Span_bar_engraver (Context *c)
  : Engraver (c)
{
}

void
Span_bar_engraver::acknowledge_bar_line (Grob_info_t<Item> info)
{
  auto *const it = info.grob ();
  if (!it->internal_has_interface (ly_symbol2scm ("span-bar-interface")))
    {
      bars_.push_back (it);

      if (bars_.size () >= 2 && !spanbar_)
        make_spanbar_ = true;
    }
}

void
Span_bar_engraver::acknowledge_span_bar_stub (Grob_info_t<Item> info)
{
  stubs_.push_back (info.grob ());
}

void
Span_bar_engraver::process_acknowledged ()
{
  if (make_spanbar_)
    {
      spanbar_ = make_item ("SpanBar", SCM_EOL);
      for (auto *const bar : bars_)
        {
          Pointer_group_interface::add_grob (spanbar_,
                                             ly_symbol2scm ("elements"), bar);
        }
      // TODO: More bar lines could be acknowledged, but they won't be added to
      // the group.  That might not happen currently, but it could conceivably
      // happen after enhancements to bar-line engraving.
      make_spanbar_ = false;
    }
}

void
Span_bar_engraver::stop_translation_timestep ()
{
  if (spanbar_)
    {
      // Span_bar_stub_engraver creates stubs in contexts where no bar line was
      // created.  Usually, this is because there is no Bar_engraver operating
      // in these contexts; however, if Bar_engraver is operating in one of
      // those contexts and has been configured not to create a BarLine at this
      // point, it sets SpanBarStub.allow-span-bar and .allow-span-bar-above to
      // #f to signal that.  In that case, we include this stub among the
      // SpanBar's elements so that the SpanBar avoids drawing through the staff
      // (or whatever context it happens to be).
      for (auto *const stub : stubs_)
        {
          if (!from_scm<bool> (get_property (stub, "allow-span-bar"))
              || !from_scm<bool> (get_property (stub, "allow-span-bar-above")))
            {
              bars_.push_back (stub);
              Pointer_group_interface::add_grob (
                spanbar_, ly_symbol2scm ("elements"), stub);
            }
        }

      // Because of alignAboveContext and alignBelowContext, grobs are not
      // necessarily announced in the order that they should be laid out, so
      // they need to be sorted.
      std::stable_sort (bars_.begin (), bars_.end (),
                        [] (const auto &a, const auto &b) {
                          return Grob::get_vertical_axis_group_index (a)
                                 < Grob::get_vertical_axis_group_index (b);
                        });

      const auto num_bars = bars_.size ();
      bool allow_above = false;
      for (vsize i = 0; i < num_bars; ++i)
        {
          const auto &bar = bars_[i];
          const bool is_bottom = ((i + 1) == num_bars);
          const bool allow_below
            = !is_bottom
              && from_scm<bool> (get_property (bar, "allow-span-bar"))
              && from_scm<bool> (
                get_property (bars_[i + 1], "allow-span-bar-above"));
          set_object (
            bar, "has-span-bar",
            scm_cons (allow_below ? spanbar_->self_scm () : SCM_BOOL_F,
                      allow_above ? spanbar_->self_scm () : SCM_BOOL_F));
          allow_above = allow_below;
        }
      spanbar_ = nullptr;
    }
  bars_.clear ();
  stubs_.clear ();
}

void
Span_bar_engraver::boot ()
{
  ADD_ACKNOWLEDGER (bar_line);
  ADD_ACKNOWLEDGER (span_bar_stub);
}

ADD_TRANSLATOR (Span_bar_engraver,
                /* doc */
                R"(
Make cross-staff bar lines: It catches all normal bar lines and draws a single
span bar across them.
                )",

                /* create */
                R"(
SpanBar
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
