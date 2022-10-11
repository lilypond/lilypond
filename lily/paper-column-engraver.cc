/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "international.hh"
#include "accidental-placement.hh"
#include "accidental-interface.hh"
#include "arpeggio.hh"
#include "axis-group-interface.hh"
#include "context.hh"
#include "note-spacing.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "separation-item.hh"
#include "staff-spacing.hh"
#include "system.hh"
#include "warn.hh"

#include "translator.icc"

using std::string;

Paper_column_engraver::Paper_column_engraver (Context *c)
  : Engraver (c)
{
}

void
Paper_column_engraver::initialize ()
{
  system_ = unsmob<System> (get_property (this, "rootSystem"));
}

void
Paper_column_engraver::start_translation_timestep ()
{
  break_events_.clear ();
  skiptypesetting_at_start_of_timestep_
    = from_scm<bool> (get_property (context (), "skipTypesetting"));
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
  for (auto *const break_event : break_events_)
    {
      string prefix;
      SCM name_sym = scm_car (get_property (break_event, "class"));
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

      SCM cur_pen = get_property (command_column_, pen_str.c_str ());
      SCM pen = get_property (break_event, "break-penalty");
      SCM perm = get_property (break_event, "break-permission");
      bool force_break_permission;

      if (!only_do_permissions && scm_is_number (pen))
        {
          Real new_pen
            = from_scm<double> (cur_pen, 0.0) + from_scm<double> (pen);
          set_property (command_column_, pen_str.c_str (), to_scm (new_pen));
          set_property (command_column_, perm_str.c_str (),
                        ly_symbol2scm ("allow"));
          force_break_permission = true;
        }
      else
        {
          set_property (command_column_, perm_str.c_str (), perm);
          force_break_permission = !scm_is_null (perm);
        }

      if (force_break_permission)
        set_property (context (), "forceBreak", SCM_BOOL_T);
    }
}

void
Paper_column_engraver::pre_process_music ()
{
  /* Use the value of skipTypesetting at the start of this time step.
     The effect is that columns are created at the beginning of a
     skipped section, and when music stops being skipped, the columns
     used are those created at the beginning of the skipped section.
     DOCME: why is this necessary? */
  if (!skiptypesetting_at_start_of_timestep_)
    {
      command_column_ = make_paper_column ("NonMusicalPaperColumn");
      set_property (context (), "currentCommandColumn",
                    command_column_->self_scm ());
      system_->add_column (command_column_);
      musical_column_ = make_paper_column ("PaperColumn");
      set_property (context (), "currentMusicalColumn",
                    musical_column_->self_scm ());
      system_->add_column (musical_column_);

      if (!system_->get_bound (LEFT)) // first time step
        {
          system_->set_bound (LEFT, command_column_);
          set_property (command_column_, "line-break-permission",
                        ly_symbol2scm ("allow"));
        }
    }

  handle_manual_breaks (false);
}

void
Paper_column_engraver::process_music ()
{
  for (auto *const label_event : label_events_)
    {
      SCM label = get_property (label_event, "page-label");
      SCM labels = get_property (command_column_, "labels");
      set_property (command_column_, "labels", scm_cons (label, labels));
    }

  /*
    We can't do this in start_translation_timestep (), since time sig
    changes won't have happened by then.
  */
  if (!measure_position (context ()).main_part_)
    {
      Moment mlen = Moment (measure_length (context ()));
      Grob *column = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      if (column)
        set_property (column, "measure-length", mlen.smobbed_copy ());
      else
        programming_error ("No command column?");
    }
}

void
Paper_column_engraver::acknowledge_item (Grob_info_t<Item> gi)
{
  items_.push_back (gi.grob ());
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
Paper_column_engraver::acknowledge_break_alignment (Grob_info gi)
{
  set_object (command_column_, "break-alignment", gi.grob ()->self_scm ());
}

void
Paper_column_engraver::stop_translation_timestep ()
{
  if (from_scm<bool> (get_property (this, "skipTypesetting")))
    return;

  // It would be safe to set "when" earlier, but there is no obvious need.
  SCM m = now_mom ().smobbed_copy ();
  set_property (command_column_, "when", m);
  set_property (musical_column_, "when", m);

  SCM mpos = get_property (this, "measurePosition");
  SCM barnum = get_property (this, "internalBarNumber");
  if (unsmob<Moment> (mpos) && scm_is_integer (barnum))
    {
      SCM where = scm_cons (barnum, mpos);

      set_property (command_column_, "rhythmic-location", where);
      set_property (musical_column_, "rhythmic-location", where);
    }

  for (auto *const elem : items_)
    {
      Grob *col
        = Item::is_non_musical (elem) ? command_column_ : musical_column_;

      if (!elem->get_x_parent ())
        elem->set_x_parent (col);
      if (!unsmob<Grob> (get_object (elem, "axis-group-parent-X")))
        set_object (elem, "axis-group-parent-X", col->self_scm ());

      if (has_interface<Accidental_placement> (elem)
          || has_interface<Arpeggio> (elem))
        Separation_item::add_conditional_item (col, elem);
      else if (!has_interface<Accidental_interface> (elem))
        Separation_item::add_item (col, elem);
    }
  items_.clear ();

  if (
    !break_allowed (context ())
    && breaks_) /* don't honour forbidBreak if it occurs on the first moment of a score */
    {
      set_property (command_column_, "page-turn-permission", SCM_EOL);
      set_property (command_column_, "page-break-permission", SCM_EOL);
      set_property (command_column_, "line-break-permission", SCM_EOL);
    }
  else if (Paper_column::is_breakable (command_column_))
    {
      breaks_++;

      if (!(breaks_ % 8))
        progress_indication ("[" + std::to_string (breaks_) + "]");
    }

  Context *score = find_score_context ();
  score->unset_property (ly_symbol2scm ("forbidBreak"));
  score->unset_property (ly_symbol2scm ("forceBreak"));

  label_events_.clear ();
}

void
Paper_column_engraver::finalize ()
{
  // At the end of the score, allow page breaks and turns by default, but...
  set_property (command_column_, "page-break-permission",
                ly_symbol2scm ("allow"));
  set_property (command_column_, "page-turn-permission",
                ly_symbol2scm ("allow"));

  // ...allow the user to override them.
  handle_manual_breaks (true);

  // On the other hand, line breaks are always allowed at the end of a score,
  // even if they try to stop us.
  if (!scm_is_symbol (get_property (command_column_, "line-break-permission")))
    {
      set_property (command_column_, "line-break-permission",
                    ly_symbol2scm ("allow"));
    }

  system_->set_bound (RIGHT, command_column_);
}

void
Paper_column_engraver::boot ()
{
  ADD_LISTENER (break);
  ADD_LISTENER (label);
  ADD_ACKNOWLEDGER (item);
  ADD_ACKNOWLEDGER (note_spacing);
  ADD_ACKNOWLEDGER (staff_spacing);
  ADD_ACKNOWLEDGER (break_alignment);
}

ADD_TRANSLATOR (Paper_column_engraver,
                /* doc */
                R"(
Take care of generating columns.

This engraver decides whether a column is breakable.  The default is that a
column is always breakable.  However, every @code{Bar_engraver} that does not
have a barline at a certain point will set @code{forbidBreaks} in the score
context to stop line breaks.  In practice, this means that you can make a break
point by creating a bar line (assuming that there are no beams or notes that
prevent a break point).
                )",

                /* create */
                R"(
PaperColumn
NonMusicalPaperColumn
                )",

                /* read */
                R"(
forbidBreak
                )",

                /* write */
                R"(
forbidBreak
forceBreak
currentCommandColumn
currentMusicalColumn
                )");
