/*
  axis-group-engraver.cc -- implement Axis_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  must_be_last_ = true;
  staffline_ = 0;
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
