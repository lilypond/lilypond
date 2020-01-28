/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "paper-column-engraver.hh"
#include "accidental-interface.hh"
#include "accidental-placement.hh"
#include "arpeggio.hh"
#include "axis-group-interface.hh"
#include "context.hh"
#include "international.hh"
#include "note-spacing.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "separation-item.hh"
#include "staff-spacing.hh"
#include "system.hh"
#include "warn.hh"

#include "translator.icc"

using std::string;

Paper_column_engraver::Paper_column_engraver (Context *c) : Engraver (c)
{
  last_moment_.main_part_ = Rational (-1, 1);
  command_column_ = 0;
  musical_column_ = 0;
  breaks_ = 0;
  system_ = 0;
  first_ = true;
  made_columns_ = false;
}

void
Paper_column_engraver::finalize ()
{
  if (!(breaks_ % 8))
    progress_indication ("[" + std::to_string (breaks_) + "]");

  if (!made_columns_)
    {
      make_columns ();
      SCM m = now_mom ().smobbed_copy ();
      command_column_->set_property ("when", m);
      musical_column_->set_property ("when", m);
    }

  if (command_column_)
    {
      // At the end of the score, allow page breaks and turns by default, but...
      command_column_->set_property ("page-break-permission",
                                     ly_symbol2scm ("allow"));
      command_column_->set_property ("page-turn-permission",
                                     ly_symbol2scm ("allow"));

      // ...allow the user to override them.
      handle_manual_breaks (true);

      // On the other hand, line breaks are always allowed at the end of a
      // score, even if they try to stop us.
      if (!scm_is_symbol (
              command_column_->get_property ("line-break-permission")))
        command_column_->set_property ("line-break-permission",
                                       ly_symbol2scm ("allow"));

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
  system_ = unsmob<System> (get_property ("rootSystem"));
  make_columns ();

  system_->set_bound (LEFT, command_column_);
  command_column_->set_property ("line-break-permission",
                                 ly_symbol2scm ("allow"));
}

void
Paper_column_engraver::acknowledge_item (Grob_info gi)
{
  items_.push_back (dynamic_cast<Item *> (gi.grob ()));
}

void
Paper_column_engraver::acknowledge_staff_spacing (Grob_info gi)
{
  Pointer_group_interface::add_grob (
      command_column_, ly_symbol2scm ("spacing-wishes"), gi.grob ());
}

void
Paper_column_engraver::acknowledge_note_spacing (Grob_info gi)
{
  Pointer_group_interface::add_grob (
      musical_column_, ly_symbol2scm ("spacing-wishes"), gi.grob ());
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
Paper_column_engraver::listen_break (Stream_event *ev)
{
  break_events_.push_back (ev);
}

void
Paper_column_engraver::listen_label (Stream_event *ev)
{
  label_events_.push_back (ev);
}

void
Paper_column_engraver::handle_manual_breaks (bool only_do_permissions)
{
  for (vsize i = 0; i < break_events_.size (); i++)
    {
      string prefix;
      SCM name_sym = scm_car (break_events_[i]->get_property ("class"));
      string name = ly_symbol2string (name_sym);
      size_t end = name.rfind ("-event");
      if (end)
        prefix = name.substr (0, end);
      else
        {
          programming_error (
              "Paper_column_engraver doesn't know about this break-event");
          return;
        }

      string perm_str = prefix + "-permission";
      string pen_str = prefix + "-penalty";

      SCM cur_pen = command_column_->get_property (pen_str.c_str ());
      SCM pen = break_events_[i]->get_property ("break-penalty");
      SCM perm = break_events_[i]->get_property ("break-permission");

      if (!only_do_permissions && scm_is_number (pen))
        {
          Real new_pen = robust_scm2double (cur_pen, 0.0) + scm_to_double (pen);
          command_column_->set_property (pen_str.c_str (),
                                         scm_from_double (new_pen));
          command_column_->set_property (perm_str.c_str (),
                                         ly_symbol2scm ("allow"));
        }
      else
        command_column_->set_property (perm_str.c_str (), perm);
    }
}

void
Paper_column_engraver::process_music ()
{
  handle_manual_breaks (false);

  for (vsize i = 0; i < label_events_.size (); i++)
    {
      SCM label = label_events_[i]->get_property ("page-label");
      SCM labels = command_column_->get_property ("labels");
      command_column_->set_property ("labels", scm_cons (label, labels));
    }

  bool start_of_measure = (last_moment_.main_part_ != now_mom ().main_part_
                           && !measure_position (context ()).main_part_);

  /*
    We can't do this in start_translation_timestep (), since time sig
    changes won't have happened by then.
  */
  if (start_of_measure)
    {
      Moment mlen = Moment (measure_length (context ()));
      Grob *column = unsmob<Grob> (get_property ("currentCommandColumn"));
      if (column)
        column->set_property ("measure-length", mlen.smobbed_copy ());
      else
        programming_error ("No command column?");
    }
}

void
Paper_column_engraver::stop_translation_timestep ()
{
  if (to_boolean (get_property ("skipTypesetting")))
    return;

  SCM m = now_mom ().smobbed_copy ();
  command_column_->set_property ("when", m);
  musical_column_->set_property ("when", m);

  SCM mpos = get_property ("measurePosition");
  SCM barnum = get_property ("internalBarNumber");
  if (unsmob<Moment> (mpos) && scm_is_integer (barnum))
    {
      SCM where = scm_cons (barnum, mpos);

      command_column_->set_property ("rhythmic-location", where);
      musical_column_->set_property ("rhythmic-location", where);
    }

  for (vsize i = 0; i < items_.size (); i++)
    {
      Item *elem = items_[i];
      Grob *col
          = Item::is_non_musical (elem) ? command_column_ : musical_column_;

      if (!elem->get_parent (X_AXIS))
        elem->set_parent (col, X_AXIS);
      if (!unsmob<Grob> (elem->get_object ("axis-group-parent-X")))
        elem->set_object ("axis-group-parent-X", col->self_scm ());

      if (has_interface<Accidental_placement> (elem)
          || has_interface<Arpeggio> (elem))
        Separation_item::add_conditional_item (col, elem);
      else if (!has_interface<Accidental_interface> (elem))
        Separation_item::add_item (col, elem);
    }
  items_.clear ();

  if (to_boolean (get_property ("forbidBreak"))
      && breaks_) /* don't honour forbidBreak if it occurs on the first moment
                     of a score */
    {
      command_column_->set_property ("page-turn-permission", SCM_EOL);
      command_column_->set_property ("page-break-permission", SCM_EOL);
      command_column_->set_property ("line-break-permission", SCM_EOL);
      for (vsize i = 0; i < break_events_.size (); i++)
        {
          SCM perm = break_events_[i]->get_property ("break-permission");
          if (scm_is_eq (perm, ly_symbol2scm ("force"))
              || scm_is_eq (perm, ly_symbol2scm ("allow")))
            warning (_ ("forced break was overridden by some other event, "
                        "should you be using bar checks?"));
        }
    }
  else if (Paper_column::is_breakable (command_column_))
    {
      breaks_++;

      if (!(breaks_ % 8))
        progress_indication ("[" + std::to_string (breaks_) + "]");
    }

  find_score_context ()->unset_property (ly_symbol2scm ("forbidBreak"));

  first_ = false;
  label_events_.clear ();
}

void
Paper_column_engraver::start_translation_timestep ()
{
  break_events_.clear ();
  if (!first_ && !to_boolean (get_property ("skipTypesetting")))
    {
      make_columns ();
      made_columns_ = true;
    }
}

void
Paper_column_engraver::boot ()
{
  ADD_LISTENER (Paper_column_engraver, break);
  ADD_LISTENER (Paper_column_engraver, label);
  ADD_ACKNOWLEDGER (Paper_column_engraver, item);
  ADD_ACKNOWLEDGER (Paper_column_engraver, note_spacing);
  ADD_ACKNOWLEDGER (Paper_column_engraver, staff_spacing);
}

ADD_TRANSLATOR (Paper_column_engraver,
                /* doc */
                "Take care of generating columns.\n"
                "\n"
                "This engraver decides whether a column is breakable.  The"
                " default is that a column is always breakable.  However,"
                " every @code{Bar_engraver} that does not have a barline at a"
                " certain point will set @code{forbidBreaks} in the score"
                " context to stop line breaks.  In practice, this means that"
                " you can make a break point by creating a bar line (assuming"
                " that there are no beams or notes that prevent a break"
                " point).",

                /* create */
                "PaperColumn "
                "NonMusicalPaperColumn ",

                /* read */
                "forbidBreak ",

                /* write */
                "forbidBreak "
                "currentCommandColumn "
                "currentMusicalColumn ");
