/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2008--2009 Han-Wen Nienhuys <hanwen@lilypond.org>
  

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

#include <set>

#include "engraver.hh"

#include "axis-group-interface.hh"
#include "directional-element-interface.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"

#include "translator.icc"

class Dynamic_align_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Dynamic_align_engraver);
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_ACKNOWLEDGER (dynamic);
  DECLARE_END_ACKNOWLEDGER (dynamic);

protected:
  virtual void stop_translation_timestep ();

private:
  void create_line_spanner (Stream_event *cause);
  Spanner* line_;

  vector<Spanner*> ended_;
  vector<Spanner*> started_;
  vector<Grob*> scripts_;
  vector<Grob*> support_;
  
  set<Spanner*> running_;
};

Dynamic_align_engraver::Dynamic_align_engraver ()
{
  line_ = 0;
}

ADD_ACKNOWLEDGER (Dynamic_align_engraver, dynamic);
ADD_ACKNOWLEDGER (Dynamic_align_engraver, note_column);
ADD_END_ACKNOWLEDGER (Dynamic_align_engraver, dynamic);

void
Dynamic_align_engraver::create_line_spanner (Stream_event* event)
{
  if (!line_)
    line_ = make_spanner ("DynamicLineSpanner",
			  event ? event->self_scm() : SCM_EOL);
}

void
Dynamic_align_engraver::acknowledge_end_dynamic (Grob_info info)
{
  if (Spanner::has_interface(info.grob()))
    ended_.push_back (info.spanner ());
}

void
Dynamic_align_engraver::acknowledge_note_column (Grob_info info)
{
  support_.push_back (info.grob ());
}

void
Dynamic_align_engraver::acknowledge_dynamic (Grob_info info)
{
  Stream_event *cause = info.event_cause ();
  create_line_spanner (cause);
  if (Spanner::has_interface(info.grob()))
    started_.push_back (info.spanner ());
  else if (info.item())
    scripts_.push_back (info.item());
  else
    info.grob ()->programming_error ("Unknown dynamic grob.");

  Axis_group_interface::add_element (line_, info.grob ());

  if (cause)
    {
      if (Direction d = to_dir (cause->get_property ("direction")))
	set_grob_direction (line_, d);
    }
}

void
Dynamic_align_engraver::stop_translation_timestep ()
{
  for (vsize i = 0; i < started_.size(); i++)
    running_.insert (started_[i]);
  for (vsize i = 0; i < ended_.size(); i++)
    {
      Spanner *sp = ended_[i];

      set<Spanner*>::iterator it  = running_.find (sp);
      if (it != running_.end())
	running_.erase (it);
      else
	started_[i]->programming_error ("Lost track of this dynamic spanner.");
    }

  bool end = line_ && running_.empty ();
  Direction d = LEFT;
  do
    {
      if (line_
	  && ((d == LEFT && !line_->get_bound (LEFT))
	      || (end && d == RIGHT && !line_->get_bound (RIGHT))))
	{
	  vector<Spanner*> const &spanners =
	    (d == LEFT) ? started_ : ended_;
	  
	  Grob *bound = 0;
	  if (scripts_.size())
	    bound = scripts_[0];
	  else if (spanners.size())
	    bound = spanners[0]->get_bound (d);
	  else
	    {
	      programming_error ("Started DynamicLineSpanner but have no left bound.");
	      bound = unsmob_grob (get_property ("currentMusicalColumn"));
	    }

	  line_->set_bound (d, bound);
	}
    }
  while (flip (&d) != LEFT);

  for (vsize i = 0; line_ && i < support_.size (); i++)
    Side_position_interface::add_support (line_, support_[i]);

  if (end) 
    line_ = 0;

  ended_.clear ();
  started_.clear ();
  scripts_.clear ();
  support_.clear ();
}


ADD_TRANSLATOR (Dynamic_align_engraver,
		/* doc */
		"Align hairpins and dynamic texts on a horizontal line",

		/* create */
		"DynamicLineSpanner ",

		/* read */
		"currentMusicalColumn ",

		/* write */
		""
		);
