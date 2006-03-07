/*
  paper-column-engraver.cc -- implement Paper_column_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-column-engraver.hh"
#include "system.hh"
#include "item.hh"
#include "paper-column.hh"
#include "staff-spacing.hh"
#include "note-spacing.hh"
#include "pointer-group-interface.hh"
#include "context.hh"
#include "axis-group-interface.hh"
#include "warn.hh"

#include "translator.icc"

Paper_column_engraver::Paper_column_engraver ()
{
  last_moment_.main_part_ = Rational (-1,1); 
  command_column_ = 0;
  musical_column_ = 0;
  breaks_ = 0;
  break_event_ = 0;
  system_ = 0;
  first_ = true;
}

void
Paper_column_engraver::finalize ()
{
  if ((breaks_ % 8))
    progress_indication ("[" + to_string (breaks_) + "]");

  if (command_column_)
    {
      command_column_->set_property ("breakable", SCM_BOOL_T);
      system_->set_bound (RIGHT, command_column_);
    }
}

void
Paper_column_engraver::make_columns ()
{
  /*
    ugh.
  */
  Paper_column *p1 = make_paper_column ("NonMusicalPaperColumn");
  Paper_column *p2 = make_paper_column ("PaperColumn");

  SCM m = now_mom ().smobbed_copy ();
  p1->set_property ("when", m);
  p2->set_property ("when", m);

  set_columns (p1, p2);
}

void
Paper_column_engraver::initialize ()
{
  system_ = dynamic_cast<System *> (unsmob_grob (get_property ("rootSystem")));
  make_columns ();

  system_->set_bound (LEFT, command_column_);
  command_column_->set_property ("breakable", SCM_BOOL_T);
}

void
Paper_column_engraver::acknowledge_item (Grob_info gi)
{
  items_.push_back (gi.item ());
}

void
Paper_column_engraver::acknowledge_staff_spacing (Grob_info gi)
{
  Pointer_group_interface::add_grob (command_column_,
				     ly_symbol2scm ("spacing-wishes"),
				     gi.grob ());
}
void
Paper_column_engraver::acknowledge_note_spacing (Grob_info gi)
{
  Pointer_group_interface::add_grob (musical_column_,
				     ly_symbol2scm ("spacing-wishes"),
				     gi.grob ());
}

void
Paper_column_engraver::set_columns (Paper_column *new_command,
				    Paper_column *new_musical)
{
  command_column_ = new_command;
  musical_column_ = new_musical;
  if (new_command)
    context ()->set_property ("currentCommandColumn", new_command->self_scm ());

  if (new_musical)
    context ()->set_property ("currentMusicalColumn", new_musical->self_scm ());

  system_->add_column (command_column_);
  system_->add_column (musical_column_);
}

void
Paper_column_engraver::forbid_breaks ()
{
  if (command_column_ && !first_)
    command_column_->set_property ("breakable", SCM_EOL);
}

bool
Paper_column_engraver::try_music (Music *m)
{
  break_event_ = m;

  return true;
}

void
Paper_column_engraver::process_music ()
{
  if (break_event_)
    {
      SCM pen = command_column_->get_property ("penalty");
      Real total_penalty = scm_is_number (pen) ? scm_to_double (pen) : 0.0;

      SCM mpen = break_event_->get_property ("penalty");
      if (scm_is_number (mpen))
	total_penalty += scm_to_double (mpen);

      command_column_->set_property ("penalty", scm_from_double (total_penalty));

      /* ugh.  arbitrary, hardcoded */
      if (total_penalty > 10000.0)
	forbid_breaks ();

      SCM page_pen = command_column_->get_property ("page-penalty");
      Real total_pp = scm_is_number (page_pen) ? scm_to_double (page_pen) : 0.0;
      SCM mpage_pen = break_event_->get_property ("page-penalty");
      if (scm_is_number (mpage_pen))
	total_pp += scm_to_double (mpage_pen);

      command_column_->set_property ("page-penalty", scm_from_double (total_pp));
    }

  bool start_of_measure = (last_moment_.main_part_ != now_mom ().main_part_
			   && !measure_position (context ()).main_part_);

  /*
    We can't do this in start_translation_timestep(), since time sig
    changes won't have happened by then.
  */
  if (start_of_measure)
    {
      Moment mlen = Moment (measure_length (context ()));
      Grob *column = unsmob_grob (get_property ("currentCommandColumn"));
      if (column)
	column->set_property ("measure-length", mlen.smobbed_copy ());
      else
	programming_error ("No command column?");
    }
}

void
Paper_column_engraver::stop_translation_timestep ()
{
  for (vsize i = 0; i < items_.size (); i++)
    {
      Item *elem = items_[i];
      if (!elem->get_parent (X_AXIS)
	  || !unsmob_grob (elem->get_object ("axis-group-parent-X")))
	{
	  bool br = to_boolean (elem->get_property ("breakable"));
	  Axis_group_interface::add_element (br ? command_column_ : musical_column_, elem);
	}
    }
  items_.clear ();

  if (to_boolean (command_column_->get_property ("breakable")))
    {
      breaks_++;
      if (! (breaks_%8))
	progress_indication ("[" + to_string (breaks_) + "]");
    }

  first_ = false;
  break_event_ = 0;
}

void
Paper_column_engraver::start_translation_timestep ()
{
  /*
    TODO: don't make columns when skipTypesetting is true.
  */
  if (!first_)
    make_columns ();
}

ADD_ACKNOWLEDGER (Paper_column_engraver, item);
ADD_ACKNOWLEDGER (Paper_column_engraver, note_spacing);
ADD_ACKNOWLEDGER (Paper_column_engraver, staff_spacing);

ADD_TRANSLATOR (Paper_column_engraver,
		/* doc */ "Takes care of generating columns."
		"\n\n "
		"This engraver decides whether a column is breakable. The default is "
		"that a column is always breakable. However, when every Bar_engraver "
		"that does not have a barline at a certain point will call "
		"Score_engraver::forbid_breaks to stop linebreaks.  In practice, this "
		"means that you can make a breakpoint by creating a barline (assuming "
		"that there are no beams or notes that prevent a breakpoint.) ",
		
		/* create */
		"PaperColumn "
		"NonMusicalPaperColumn",
		
		/* accept */ "break-event",
		/* read */ "",
		/* write */
		"currentCommandColumn "
		"currentMusicalColumn");
