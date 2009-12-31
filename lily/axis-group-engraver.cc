/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "axis-group-engraver.hh"

#include "axis-group-interface.hh"
#include "pointer-group-interface.hh"
#include "context.hh"
#include "international.hh"
#include "spanner.hh"
#include "warn.hh"

#include "translator.icc"

Axis_group_engraver::Axis_group_engraver ()
{
  staffline_ = 0;
}

bool
Axis_group_engraver::must_be_last () const
{
  return true;
}

void
Axis_group_engraver::process_music ()
{
  if (!staffline_)
    {
      staffline_ = get_spanner ();
      Grob *it = unsmob_grob (get_property ("currentCommandColumn"));
      staffline_->set_bound (LEFT, it);
    }
}

Spanner *
Axis_group_engraver::get_spanner ()
{
  return make_spanner ("VerticalAxisGroup", SCM_EOL);
}

void
Axis_group_engraver::finalize ()
{
  if (staffline_)
    {
      Grob *it = unsmob_grob (get_property ("currentCommandColumn"));
      staffline_->set_bound (RIGHT, it);

      Pointer_group_interface::set_ordered (staffline_, ly_symbol2scm ("elements"), false);
    }
}

void
Axis_group_engraver::acknowledge_grob (Grob_info i)
{
  elts_.push_back (i.grob ());
}

/*
  maybe should check if our parent is set, because we now get a
  cyclic parent relationship if we have two Axis_group_engravers in
  the context.  */
void
Axis_group_engraver::process_acknowledged ()
{
  if (!staffline_)
    return;

  for (vsize i = 0; i < elts_.size (); i++)
    {
      if (!unsmob_grob (elts_[i]->get_object ("axis-group-parent-Y")))
	{
	  if (staffline_->get_parent (Y_AXIS)
	      && staffline_->get_parent (Y_AXIS) == elts_[i])
	    {
	      staffline_->warning (_ ("Axis_group_engraver: vertical group already has a parent"));
	      staffline_->warning (_ ("are there two Axis_group_engravers?"));
	      staffline_->warning (_ ("removing this vertical group"));
	      staffline_->suicide ();
	      staffline_ = 0;
	      break;
	    }
	  add_element (elts_[i]);
	}
    }
  elts_.clear ();
}

void
Axis_group_engraver::add_element (Grob *e)
{
  Axis_group_interface::add_element (staffline_, e);
}

ADD_ACKNOWLEDGER (Axis_group_engraver, grob);

ADD_TRANSLATOR (Axis_group_engraver,
		/* doc */
		"Group all objects created in this context in a"
		" @code{VerticalAxisGroup} spanner.",

		/* create */
		"VerticalAxisGroup ",

		/* read */
		"currentCommandColumn ",

		/* write */
		""
		);
