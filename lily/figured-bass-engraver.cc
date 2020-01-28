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

#include "engraver.hh"

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "context.hh"
#include "grob-array.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

using std::vector;

struct Figure_group
{
  Spanner *group_;
  Spanner *continuation_line_;

  SCM number_;
  SCM alteration_;
  SCM augmented_;
  SCM diminished_;
  SCM augmented_slash_;
  SCM text_;

  Item *figure_item_;
  Stream_event *current_event_;

  Figure_group ()
  {
    figure_item_ = 0;
    continuation_line_ = 0;
    reset_figure ();
    group_ = 0;
    current_event_ = 0;
  }
  /* Reset (or init) all figure information to FALSE */
  void reset_figure ()
  {
    number_ = SCM_BOOL_F;
    alteration_ = SCM_BOOL_F;
    augmented_ = SCM_BOOL_F;
    diminished_ = SCM_BOOL_F;
    augmented_slash_ = SCM_BOOL_F;
    text_ = SCM_BOOL_F;
  }
  /* Mark the members of the struct as used for the GUILE Garbage Collection */
  void gc_mark () const
  {
    scm_gc_mark (number_);
    scm_gc_mark (alteration_);
    scm_gc_mark (augmented_);
    scm_gc_mark (diminished_);
    scm_gc_mark (augmented_slash_);
    scm_gc_mark (text_);
  }
  bool group_is_equal_to (Stream_event *evt) const
  {
    return ly_is_equal (number_, evt->get_property ("figure"))
           && ly_is_equal (alteration_, evt->get_property ("alteration"))
           && ly_is_equal (augmented_, evt->get_property ("augmented"))
           && ly_is_equal (diminished_, evt->get_property ("diminished"))
           && ly_is_equal (augmented_slash_,
                           evt->get_property ("augmented-slash"))
           && ly_is_equal (text_, evt->get_property ("text"));
  }
  bool is_continuation () const
  {
    return current_event_ && group_is_equal_to (current_event_);
  }
  void assign_from_event (Stream_event *currevt, Item *item)
  {
    number_ = current_event_->get_property ("figure");
    alteration_ = currevt->get_property ("alteration");
    augmented_ = currevt->get_property ("augmented");
    diminished_ = currevt->get_property ("diminished");
    augmented_slash_ = currevt->get_property ("augmented-slash");
    text_ = currevt->get_property ("text");
    figure_item_ = item;
  }
};

struct Figured_bass_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Figured_bass_engraver);
  void clear_spanners ();
  void add_brackets ();
  void create_grobs ();

  void center_continuations (vector<Spanner *> const &consecutive_lines);
  void center_repeated_continuations ();

protected:
  vector<Figure_group> groups_;
  Spanner *alignment_;
  vector<Stream_event *> new_events_;
  bool continuation_;
  bool new_event_found_;

  Moment stop_moment_;
  bool have_rest_;

  void listen_rest (Stream_event *);
  void listen_bass_figure (Stream_event *);

  void derived_mark () const override;

  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();
};

Figured_bass_engraver::Figured_bass_engraver (Context *c) : Engraver (c)
{
  alignment_ = 0;
  continuation_ = false;
  have_rest_ = 0;
  new_event_found_ = false;
}

void
Figured_bass_engraver::derived_mark () const
{
  for (vsize i = 0; i < groups_.size (); i++)
    {
      groups_[i].gc_mark ();
    }
}

void
Figured_bass_engraver::start_translation_timestep ()
{
  if (now_mom ().main_part_ < stop_moment_.main_part_
      || now_mom ().grace_part_ < Rational (0))
    return;

  have_rest_ = 0;
  new_events_.clear ();
  for (vsize i = 0; i < groups_.size (); i++)
    groups_[i].current_event_ = 0;

  continuation_ = false;
}

void
Figured_bass_engraver::stop_translation_timestep ()
{
  if (groups_.empty () || now_mom ().main_part_ < stop_moment_.main_part_
      || now_mom ().grace_part_ < Rational (0))
    return;

  bool found = false;
  for (vsize i = 0; !found && i < groups_.size (); i++)
    found = found || groups_[i].current_event_;

  if (!found)
    clear_spanners ();
}

void
Figured_bass_engraver::listen_rest (Stream_event *)
{
  have_rest_ = true;
}

void
Figured_bass_engraver::listen_bass_figure (Stream_event *ev)
{
  new_event_found_ = true;
  Moment stop = now_mom () + get_event_length (ev, now_mom ());
  stop_moment_ = std::max (stop_moment_, stop);

  // Handle no-continuation here, don't even add it to the already existing
  // spanner... This fixes some layout issues (figure will be placed separately)
  bool no_continuation = to_boolean (ev->get_property ("no-continuation"));
  if (to_boolean (get_property ("useBassFigureExtenders")) && !no_continuation)
    {
      for (vsize i = 0; i < groups_.size (); i++)
        {
          if (!groups_[i].current_event_ && groups_[i].group_is_equal_to (ev))
            {
              groups_[i].current_event_ = ev;
              continuation_ = true;
              return;
            }
        }
    }
  new_events_.push_back (ev);
}

void
Figured_bass_engraver::center_continuations (
    vector<Spanner *> const &consecutive_lines)
{
  vector<Grob *> left_figs;
  for (vsize j = consecutive_lines.size (); j--;)
    left_figs.push_back (consecutive_lines[j]->get_bound (LEFT));

  SCM ga = Grob_array::make_array ();
  unsmob<Grob_array> (ga)->set_array (left_figs);

  for (vsize j = consecutive_lines.size (); j--;)
    consecutive_lines[j]->set_object ("figures",
                                      unsmob<Grob_array> (ga)->smobbed_copy ());
}

void
Figured_bass_engraver::center_repeated_continuations ()
{
  vector<Spanner *> consecutive_lines;
  for (vsize i = 0; i <= groups_.size (); i++)
    {
      if (i < groups_.size () && groups_[i].continuation_line_
          && (consecutive_lines.empty ()
              || (consecutive_lines[0]->get_bound (LEFT)->get_column ()
                      == groups_[i]
                             .continuation_line_->get_bound (LEFT)
                             ->get_column ()
                  && consecutive_lines[0]->get_bound (RIGHT)->get_column ()
                         == groups_[i]
                                .continuation_line_->get_bound (RIGHT)
                                ->get_column ())))
        consecutive_lines.push_back (groups_[i].continuation_line_);
      else
        {
          center_continuations (consecutive_lines);
          consecutive_lines.clear ();
        }
    }
}

void
Figured_bass_engraver::clear_spanners ()
{
  if (!alignment_)
    return;

  announce_end_grob (alignment_, SCM_EOL);
  alignment_ = 0;

  if (to_boolean (get_property ("figuredBassCenterContinuations")))
    center_repeated_continuations ();

  for (vsize i = 0; i < groups_.size (); i++)
    {
      if (groups_[i].group_)
        {
          announce_end_grob (groups_[i].group_, SCM_EOL);
          groups_[i].group_ = 0;
        }

      if (groups_[i].continuation_line_)
        {
          announce_end_grob (groups_[i].continuation_line_, SCM_EOL);
          groups_[i].continuation_line_ = 0;
        }
    }

  /* Check me, groups_.clear () ? */
}

void
Figured_bass_engraver::process_music ()
{
  bool use_extenders = to_boolean (get_property ("useBassFigureExtenders"));
  if (alignment_ && !use_extenders)
    clear_spanners ();

  // If we have a rest, or we have no new or continued events, clear all
  // spanners
  bool ignore_rest = to_boolean (get_property ("ignoreFiguredBassRest"));
  if ((ignore_rest && have_rest_) || (!continuation_ && new_events_.empty ()))
    {
      clear_spanners ();
      groups_.clear ();
      return;
    }

  if (!new_event_found_)
    return;

  new_event_found_ = false;

  /*
    Don't need to sync alignments, if we're not using extenders.
   */
  if (!use_extenders)
    {
      clear_spanners ();
    }

  if (!continuation_)
    {
      clear_spanners ();
      groups_.clear ();
    }

  vsize k = 0;
  for (vsize i = 0; i < new_events_.size (); i++)
    {
      while (k < groups_.size () && groups_[k].current_event_)
        k++;

      if (k >= groups_.size ())
        {
          Figure_group group;
          groups_.push_back (group);
        }

      groups_[k].reset_figure ();
      groups_[k].current_event_ = new_events_[i];
      groups_[k].figure_item_ = 0;
      k++;
    }

  for (vsize i = 0; i < groups_.size (); i++)
    {
      if (!groups_[i].is_continuation ())
        {
          groups_[i].reset_figure ();
        }
    }

  if (use_extenders)
    {
      vector<int> junk_continuations;
      for (vsize i = 0; i < groups_.size (); i++)
        {
          Figure_group &group = groups_[i];

          if (group.is_continuation ())
            {
              if (!group.continuation_line_)
                {
                  Spanner *line
                      = make_spanner ("BassFigureContinuation", SCM_EOL);
                  Item *item = group.figure_item_;
                  group.continuation_line_ = line;
                  line->set_bound (LEFT, item);

                  /*
                    Don't add as child. This will cache the wrong
                    (pre-break) stencil when callbacks are triggered.
                  */
                  line->set_parent (group.group_, Y_AXIS);
                  Pointer_group_interface::add_grob (
                      line, ly_symbol2scm ("figures"), item);

                  group.figure_item_ = 0;
                }
            }
          else if (group.continuation_line_)
            junk_continuations.push_back (i);
        }

      /*
        Ugh, repeated code.
       */
      vector<Spanner *> consecutive;
      if (to_boolean (get_property ("figuredBassCenterContinuations")))
        {
          for (vsize i = 0; i <= junk_continuations.size (); i++)
            {
              if (i < junk_continuations.size ()
                  && (i == 0
                      || junk_continuations[i - 1]
                             == junk_continuations[i] - 1))
                consecutive.push_back (
                    groups_[junk_continuations[i]].continuation_line_);
              else
                {
                  center_continuations (consecutive);
                  consecutive.clear ();
                  if (i < junk_continuations.size ())
                    consecutive.push_back (
                        groups_[junk_continuations[i]].continuation_line_);
                }
            }
        }
      for (vsize i = 0; i < junk_continuations.size (); i++)
        groups_[junk_continuations[i]].continuation_line_ = 0;
    }

  create_grobs ();
  add_brackets ();
}

void
Figured_bass_engraver::create_grobs ()
{
  Grob *muscol = unsmob<Item> (get_property ("currentMusicalColumn"));
  if (!alignment_)
    {
      alignment_ = make_spanner ("BassFigureAlignment", SCM_EOL);
      alignment_->set_bound (LEFT, muscol);
    }
  alignment_->set_bound (RIGHT, muscol);

  SCM proc = get_property ("figuredBassFormatter");
  for (vsize i = 0; i < groups_.size (); i++)
    {
      Figure_group &group = groups_[i];

      if (group.current_event_)
        {
          Item *item
              = make_item ("BassFigure", group.current_event_->self_scm ());
          group.assign_from_event (group.current_event_, item);

          if (!group.group_)
            {
              group.group_ = make_spanner ("BassFigureLine", SCM_EOL);
              group.group_->set_bound (LEFT, muscol);
              Align_interface::add_element (alignment_, group.group_);
            }

          if (scm_is_true (scm_memq (group.number_,
                                     get_property ("implicitBassFigures"))))
            {
              item->set_property ("transparent", SCM_BOOL_T);
              item->set_property ("implicit", SCM_BOOL_T);
            }

          SCM text = group.text_;
          if (!Text_interface::is_markup (text) && ly_is_procedure (proc))
            {
              text = scm_call_3 (proc, group.number_,
                                 group.current_event_->self_scm (),
                                 context ()->self_scm ());
            }

          item->set_property ("text", text);

          Axis_group_interface::add_element (group.group_, item);
        }

      if (group.continuation_line_)
        {
          /*
            UGH should connect to the bass staff, and get the note heads.
            For now, simply set the hidden figure to a default value to
            ensure the extenders of different figures always end at the same
            position, e.g. in <12 5> <12 5>
          */
          group.figure_item_->set_property ("transparent", SCM_BOOL_T);
          group.figure_item_->set_property ("text", ly_string2scm ("0"));
          group.continuation_line_->set_bound (RIGHT, group.figure_item_);
        }

      if (groups_[i].group_)
        groups_[i].group_->set_bound (RIGHT, muscol);
    }
}

void
Figured_bass_engraver::add_brackets ()
{
  vector<Grob *> encompass;
  bool inside = false;
  for (vsize i = 0; i < groups_.size (); i++)
    {
      if (!groups_[i].current_event_)
        continue;

      if (to_boolean (
              groups_[i].current_event_->get_property ("bracket-start")))
        inside = true;

      if (inside && groups_[i].figure_item_)
        encompass.push_back (groups_[i].figure_item_);

      if (to_boolean (groups_[i].current_event_->get_property ("bracket-stop")))
        {
          inside = false;

          Item *brack = make_item ("BassFigureBracket",
                                   groups_[i].current_event_->self_scm ());
          for (vsize j = 0; j < encompass.size (); j++)
            {
              Pointer_group_interface::add_grob (
                  brack, ly_symbol2scm ("elements"), encompass[j]);
            }
          encompass.clear ();
        }
    }
}

void
Figured_bass_engraver::boot ()
{
  ADD_LISTENER (Figured_bass_engraver, rest);
  ADD_LISTENER (Figured_bass_engraver, bass_figure);
}

ADD_TRANSLATOR (Figured_bass_engraver,
                /* doc */
                "Make figured bass numbers.",

                /* create */
                "BassFigure "
                "BassFigureAlignment "
                "BassFigureBracket "
                "BassFigureContinuation "
                "BassFigureLine ",

                /* read */
                "figuredBassAlterationDirection "
                "figuredBassCenterContinuations "
                "figuredBassFormatter "
                "implicitBassFigures "
                "useBassFigureExtenders "
                "ignoreFiguredBassRest ",

                /* write */
                "");
