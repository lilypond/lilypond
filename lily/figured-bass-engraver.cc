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

#include "engraver.hh"

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "context.hh"
#include "grob-array.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "simple-event-listener.hh"
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
    return ly_is_equal (number_, get_property (evt, "figure"))
           && ly_is_equal (alteration_, get_property (evt, "alteration"))
           && ly_is_equal (augmented_, get_property (evt, "augmented"))
           && ly_is_equal (diminished_, get_property (evt, "diminished"))
           && ly_is_equal (augmented_slash_,
                           get_property (evt, "augmented-slash"))
           && ly_is_equal (text_, get_property (evt, "text"));
  }
  bool is_continuation () const
  {
    return current_event_ && group_is_equal_to (current_event_);
  }
  void assign_from_event (Stream_event *currevt, Item *item)
  {
    number_ = get_property (current_event_, "figure");
    alteration_ = get_property (currevt, "alteration");
    augmented_ = get_property (currevt, "augmented");
    diminished_ = get_property (currevt, "diminished");
    augmented_slash_ = get_property (currevt, "augmented-slash");
    text_ = get_property (currevt, "text");
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
  Boolean_event_listener rest_listener_;

  void listen_bass_figure (Stream_event *);

  void derived_mark () const override;

  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();
};

Figured_bass_engraver::Figured_bass_engraver (Context *c)
  : Engraver (c)
{
  alignment_ = 0;
  continuation_ = false;
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

  rest_listener_.reset ();
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
Figured_bass_engraver::listen_bass_figure (Stream_event *ev)
{
  new_event_found_ = true;
  auto stop = now_mom () + get_event_length (ev, now_mom ());
  stop_moment_ = std::max (stop_moment_, stop);

  // Handle no-continuation here, don't even add it to the already existing
  // spanner... This fixes some layout issues (figure will be placed separately)
  if (from_scm<bool> (get_property (this, "useBassFigureExtenders"))
      && !from_scm<bool> (get_property (ev, "no-continuation")))
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
    set_object (consecutive_lines[j], "figures",
                unsmob<Grob_array> (ga)->smobbed_copy ());
}

void
Figured_bass_engraver::center_repeated_continuations ()
{
  vector<Spanner *> consecutive_lines;
  for (auto &group : groups_)
    {
      auto *const cont_line = group.continuation_line_;
      if (cont_line
          && (consecutive_lines.empty ()
              || (consecutive_lines[0]->get_bound (LEFT)->get_column ()
                    == cont_line->get_bound (LEFT)->get_column ()
                  && consecutive_lines[0]->get_bound (RIGHT)->get_column ()
                       == cont_line->get_bound (RIGHT)->get_column ())))
        {
          consecutive_lines.push_back (cont_line);
        }
      else
        {
          center_continuations (consecutive_lines);
          consecutive_lines.clear ();
        }
    }

  center_continuations (consecutive_lines);
}

void
Figured_bass_engraver::clear_spanners ()
{
  if (!alignment_)
    return;

  announce_end_grob (alignment_, SCM_EOL);
  alignment_ = 0;

  if (from_scm<bool> (get_property (this, "figuredBassCenterContinuations")))
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
  bool use_extenders
    = from_scm<bool> (get_property (this, "useBassFigureExtenders"));
  if (alignment_ && !use_extenders)
    clear_spanners ();

  // If we have a rest, or we have no new or continued events, clear all spanners
  if ((!continuation_ && new_events_.empty ())
      || (rest_listener_.heard ()
          && from_scm<bool> (get_property (this, "ignoreFiguredBassRest"))))
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
      vector<vsize> junk_continuations;
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
                  line->set_y_parent (group.group_);
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
      if (from_scm<bool> (
            get_property (this, "figuredBassCenterContinuations")))
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
  Grob *muscol = unsmob<Item> (get_property (this, "currentMusicalColumn"));
  if (!alignment_)
    {
      alignment_ = make_spanner ("BassFigureAlignment", SCM_EOL);
      alignment_->set_bound (LEFT, muscol);
    }
  alignment_->set_bound (RIGHT, muscol);

  SCM proc = get_property (this, "figuredBassFormatter");
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

          if (scm_is_true (scm_memq (
                group.number_, get_property (this, "implicitBassFigures"))))
            {
              set_property (item, "transparent", SCM_BOOL_T);
              set_property (item, "implicit", SCM_BOOL_T);
            }

          SCM text = group.text_;
          if (!Text_interface::is_markup (text) && ly_is_procedure (proc))
            {
              text = ly_call (proc, group.number_,
                              group.current_event_->self_scm (),
                              context ()->self_scm ());
            }

          set_property (item, "text", text);

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
          set_property (group.figure_item_, "transparent", SCM_BOOL_T);
          set_property (group.figure_item_, "text", ly_string2scm ("0"));
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

      if (from_scm<bool> (
            get_property (groups_[i].current_event_, "bracket-start")))
        inside = true;

      if (inside && groups_[i].figure_item_)
        encompass.push_back (groups_[i].figure_item_);

      if (from_scm<bool> (
            get_property (groups_[i].current_event_, "bracket-stop")))
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
  ADD_DELEGATE_LISTENER (rest);
  ADD_LISTENER (bass_figure);
}

ADD_TRANSLATOR (Figured_bass_engraver,
                /* doc */
                R"(
Make figured bass numbers.
                )",

                /* create */
                R"(
BassFigure
BassFigureAlignment
BassFigureBracket
BassFigureContinuation
BassFigureLine
                )",

                /* read */
                R"(
figuredBassAlterationDirection
figuredBassCenterContinuations
figuredBassFormatter
implicitBassFigures
useBassFigureExtenders
ignoreFiguredBassRest
                )",

                /* write */
                R"(

                )");
