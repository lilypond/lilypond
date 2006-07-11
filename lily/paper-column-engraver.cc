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
  system_ = 0;
  first_ = true;
  last_breakable_column_ = 0;
  last_breakable_moment_ = Moment (-1);
}

void
Paper_column_engraver::finalize ()
{
  if ((breaks_ % 8))
    progress_indication ("[" + to_string (breaks_) + "]");

  if (command_column_)
    {
      command_column_->set_property ("line-break-permission", ly_symbol2scm ("allow"));
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
  /* 
     The columns are timestamped with now_mom () in
     stop_translation_timestep. Cannot happen now, because the
     first column is sometimes created before now_mom is initialised.
  */

  set_columns (p1, p2);
}

void
Paper_column_engraver::initialize ()
{
  system_ = dynamic_cast<System *> (unsmob_grob (get_property ("rootSystem")));
  make_columns ();

  system_->set_bound (LEFT, command_column_);
  command_column_->set_property ("line-break-permission", ly_symbol2scm ("allow"));
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

bool
Paper_column_engraver::try_music (Music *m)
{
  break_events_.push_back (m);

  return true;
}

void
Paper_column_engraver::process_music ()
{
  for (vsize i = 0; i < break_events_.size (); i++)
    {
      string prefix;
      SCM name = break_events_[i]->get_property ("name");
      if (name == ly_symbol2scm ("LineBreakEvent"))
	prefix = "line-break";
      else if (name == ly_symbol2scm ("PageBreakEvent"))
	prefix = "page-break";
      else if (name == ly_symbol2scm ("PageTurnEvent"))
	prefix = "page-turn";
      else
	{
	  programming_error ("Paper_column_engraver doesn't know about this break-event");
	  return;
	}
      string perm_str = prefix + "-permission";
      string pen_str = prefix + "-penalty";

      SCM cur_pen = command_column_->get_property (pen_str.c_str ());
      SCM pen = break_events_[i]->get_property ("break-penalty");
      SCM perm = break_events_[i]->get_property ("break-permission");

      if (scm_is_number (pen))
	{
	  Real new_pen = robust_scm2double (cur_pen, 0.0) + scm_to_double (pen);
	  command_column_->set_property (pen_str.c_str (), scm_from_double (new_pen));
	  command_column_->set_property (perm_str.c_str (), ly_symbol2scm ("allow"));
	}
      else
	command_column_->set_property (perm_str.c_str (), perm);
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
  SCM m = now_mom ().smobbed_copy ();
  command_column_->set_property ("when", m);
  musical_column_->set_property ("when", m);

  for (vsize i = 0; i < items_.size (); i++)
    {
      Item *elem = items_[i];
      if (!elem->get_parent (X_AXIS)
	  || !unsmob_grob (elem->get_object ("axis-group-parent-X")))
	{
	  bool br = Item::is_non_musical (elem);
	  Axis_group_interface::add_element (br ? command_column_ : musical_column_, elem);
	}
    }
  items_.clear ();

  if (to_boolean (get_property ("forbidBreak")))
    command_column_->set_property ("line-break-permission", SCM_EOL);
  else if (Paper_column::is_breakable (command_column_))
    {
      breaks_++;
      last_breakable_column_ = command_column_;
      last_breakable_moment_ = now_mom ();
      if (! (breaks_%8))
	progress_indication ("[" + to_string (breaks_) + "]");
    }

  SCM page_br = get_property ("allowPageTurn");
  if (scm_is_pair (page_br) && last_breakable_moment_ >= Rational (0))
    {
      SCM pen = scm_cdr (page_br);
      Moment *m = unsmob_moment (scm_car (page_br));
      if (m && scm_is_number (pen) && *m <= last_breakable_moment_)
	{
	  last_breakable_column_->set_property ("page-turn-permission", ly_symbol2scm ("allow"));
	  last_breakable_column_->set_property ("page-turn-penalty", pen);
	}
    }

  context ()->get_score_context ()->unset_property (ly_symbol2scm ("forbidBreak"));
  context ()->get_score_context ()->unset_property (ly_symbol2scm ("allowPageTurn"));

  first_ = false;
  break_events_.clear ();
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
		"that a column is always breakable. However, every Bar_engraver "
		"that does not have a barline at a certain point will set forbidBreaks "
                "in the score context to stop linebreaks.  In practice, this "
		"means that you can make a breakpoint by creating a barline (assuming "
		"that there are no beams or notes that prevent a breakpoint.) ",
		
		/* create */
		"PaperColumn "
		"NonMusicalPaperColumn",
		
		/* accept */ "break-event",
		/* read */
                "forbidBreak "
                "allowPageTurn",
		/* write */
                "forbidBreak "
                "allowPageTurn "
		"currentCommandColumn "
		"currentMusicalColumn");
