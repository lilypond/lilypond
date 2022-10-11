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

#include "item.hh"
#include "engraver.hh"
#include "pointer-group-interface.hh"
#include "lily-imports.hh"

#include "translator.icc"

using std::vector;

/**

Make bars that span multiple "staves". Catch bars, and span a
Span_bar over them if we find more than 2 bars.  Vertical alignment
of staves changes the appearance of spanbars.  It is up to the
aligner (Vertical_align_engraver, in this case, to add extra
dependencies to the spanbars.
*/
class Span_bar_engraver : public Engraver
{
  Item *spanbar_;
  bool make_spanbar_;
  vector<Item *> bars_;

public:
  TRANSLATOR_DECLARATIONS (Span_bar_engraver);

protected:
  void acknowledge_bar_line (Grob_info_t<Item>);
  void stop_translation_timestep ();
  void process_acknowledged ();
};

Span_bar_engraver::Span_bar_engraver (Context *c)
  : Engraver (c)
{
  spanbar_ = 0;
  make_spanbar_ = false;
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
Span_bar_engraver::process_acknowledged ()
{
  if (make_spanbar_)
    {
      spanbar_ = make_item ("SpanBar", SCM_EOL);

      for (vsize i = 0; i < bars_.size (); i++)
        Pointer_group_interface::add_grob (spanbar_, ly_symbol2scm ("elements"),
                                           bars_[i]);
      make_spanbar_ = false;
    }
}

void
Span_bar_engraver::stop_translation_timestep ()
{
  if (spanbar_)
    {
      for (vsize i = 0; i < bars_.size (); i++)
        set_object (
          bars_[i], "has-span-bar",
          scm_cons (i == bars_.size () - 1 ? SCM_BOOL_F : spanbar_->self_scm (),
                    i == 0 ? SCM_BOOL_F : spanbar_->self_scm ()));
      spanbar_ = 0;
    }
  bars_.resize (0);
}

void
Span_bar_engraver::boot ()
{
  ADD_ACKNOWLEDGER (bar_line);
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
