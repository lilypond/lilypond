/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "bar-line.hh"
#include "item.hh"
#include "span-bar.hh"
#include "engraver.hh"

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
  vector<Item*> bars_;

public:
  TRANSLATOR_DECLARATIONS (Span_bar_engraver);
protected:
  DECLARE_ACKNOWLEDGER (bar_line);
  void stop_translation_timestep ();
};

Span_bar_engraver::Span_bar_engraver ()
{
  spanbar_ = 0;
}

void
Span_bar_engraver::acknowledge_bar_line (Grob_info i)
{
  int depth = i.origin_contexts (this).size ();
  if (depth && !Span_bar::has_interface (i.grob ()))
    {
      Item *it = dynamic_cast<Item *> (i.grob ());
      bars_.push_back (it);

      if (bars_.size () >= 2 && !spanbar_)
	{
	  spanbar_ = make_item ("SpanBar", SCM_EOL);

	  spanbar_->set_parent (bars_[0], X_AXIS);
	}
    }
}

void
Span_bar_engraver::stop_translation_timestep ()
{
  if (spanbar_)
    {
      for (vsize i = 0; i < bars_.size (); i++)
	Span_bar::add_bar (spanbar_, bars_[i]);

      SCM vissym = ly_symbol2scm ("break-visibility");
      SCM vis = bars_[0]->internal_get_property (vissym);
      if (ly_is_equal (spanbar_->internal_get_property (vissym), vis))
	spanbar_->set_property (vissym, vis);

      spanbar_ = 0;
    }
  bars_.resize (0);
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Span_bar_engraver, bar_line);
ADD_TRANSLATOR (Span_bar_engraver,
		/* doc */
		"Make cross-staff bar lines: It catches all normal bar lines"
		" and draws a single span bar across them.",

		/* create */
		"SpanBar ",

		/* read */
		"",

		/* write */
		""
		);
