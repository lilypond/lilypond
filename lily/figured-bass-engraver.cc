/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>


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
  bool force_no_continuation_;
  
  Figure_group ()
  {
    figure_item_ = 0;
    force_no_continuation_ = false;
    continuation_line_ = 0;
    number_ = SCM_EOL;
    alteration_ = SCM_EOL;
    augmented_ = SCM_EOL;
    diminished_ = SCM_EOL;
    augmented_slash_ = SCM_EOL;
    text_ = SCM_EOL;
    group_ = 0;
    current_event_ = 0;
  }
  bool is_continuation () const
  {
    return
      current_event_
      && !force_no_continuation_
      && ly_is_equal (number_,
		      current_event_->get_property ("figure"))
      && ly_is_equal (alteration_,
		      current_event_->get_property ("alteration"))
      && ly_is_equal (augmented_,
		      current_event_->get_property ("augmented"))
      && ly_is_equal (diminished_,
		      current_event_->get_property ("diminished"))
      && ly_is_equal (augmented_slash_,
		      current_event_->get_property ("augmented-slash"))
      && ly_is_equal (text_,
		      current_event_->get_property ("text"));
  }
};

struct Figured_bass_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Figured_bass_engraver);
  void clear_spanners ();
  void add_brackets ();
  void create_grobs ();

  void center_continuations (vector<Spanner*> const &consecutive_lines);
  void center_repeated_continuations ();
protected:
  vector<Figure_group> groups_;
  Spanner *alignment_;
  vector<Stream_event *> new_events_;
  bool continuation_;
  bool new_event_found_;
  
  Moment stop_moment_;
  Stream_event *rest_event_; 

  DECLARE_TRANSLATOR_LISTENER (rest);
  DECLARE_TRANSLATOR_LISTENER (bass_figure);

  virtual void derived_mark () const; 

  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();
};

void
Figured_bass_engraver::derived_mark () const
{
  for (vsize i = 0; i < groups_.size (); i++)
    {
      scm_gc_mark (groups_[i].number_);
      scm_gc_mark (groups_[i].alteration_);
      scm_gc_mark (groups_[i].augmented_);
      scm_gc_mark (groups_[i].diminished_);
      scm_gc_mark (groups_[i].augmented_slash_);
      scm_gc_mark (groups_[i].text_);
    }
}

void
Figured_bass_engraver::stop_translation_timestep ()
{
  if (groups_.empty ()
      || now_mom ().main_part_ < stop_moment_.main_part_
      || now_mom ().grace_part_ < Rational (0))
    return ;
  
  bool found = false;
  for (vsize i = 0; !found && i < groups_.size (); i++)
    found  = found  || groups_[i].current_event_;

  if (!found)
    clear_spanners ();
}

Figured_bass_engraver::Figured_bass_engraver ()
{
  alignment_ = 0;
  continuation_ = false;
  rest_event_ = 0;
  new_event_found_ = false;
}

void
Figured_bass_engraver::start_translation_timestep ()
{
  if (now_mom ().main_part_ < stop_moment_.main_part_
      || now_mom ().grace_part_ < Rational (0))
    return ;
  
  rest_event_ = 0;
  new_events_.clear ();
  for (vsize i = 0; i < groups_.size (); i++)
    groups_[i].current_event_ = 0;

  continuation_ = false;

  
}

IMPLEMENT_TRANSLATOR_LISTENER (Figured_bass_engraver, rest);
void
Figured_bass_engraver::listen_rest (Stream_event *ev)
{
  if (to_boolean (get_property ("ignoreFiguredBassRest")))
    {
      new_event_found_ = true;

      /*
	No ASSIGN_EVENT_ONCE () ; otherwise we get warnings about
	polyphonic rests.
       */
      rest_event_ = ev;
    }
}

IMPLEMENT_TRANSLATOR_LISTENER (Figured_bass_engraver, bass_figure);
void
Figured_bass_engraver::listen_bass_figure (Stream_event *ev)
{
  new_event_found_ = true;
  Moment stop  = now_mom () + get_event_length (ev, now_mom ());
  stop_moment_ = max (stop_moment_, stop);

  if (to_boolean (get_property ("useBassFigureExtenders")))
    {
      SCM fig = ev->get_property ("figure");
      SCM txt = ev->get_property ("text");
      for (vsize i = 0; i < groups_.size (); i++)
	{
	  if (!groups_[i].current_event_
	      && ly_is_equal (groups_[i].number_, fig)
	      && ly_is_equal (groups_[i].text_, txt))
	    {
	      groups_[i].current_event_ = ev;
	      groups_[i].force_no_continuation_
		= to_boolean (ev->get_property ("no-continuation"));
	      continuation_ = true;
	      return; 
	    }
	}
    }  
  new_events_.push_back (ev);
}

void
Figured_bass_engraver::center_continuations (vector<Spanner*> const &consecutive_lines)
{
  if (consecutive_lines.size () == 2)
    {
      vector<Grob*> left_figs;
      for (vsize j = consecutive_lines.size (); j--;)
	left_figs.push_back (consecutive_lines[j]->get_bound (LEFT));

      SCM  ga = Grob_array::make_array ();
      unsmob_grob_array (ga)->set_array (left_figs);

      for (vsize j = consecutive_lines.size (); j--;)
	consecutive_lines[j]->set_object ("figures",
					  unsmob_grob_array (ga)->smobbed_copy ());
    }
}

void
Figured_bass_engraver::center_repeated_continuations ()
{  
  vector<Spanner*> consecutive_lines;
  for (vsize i = 0; i <= groups_.size (); i++)
    {
      if (i < groups_.size ()
	  && groups_[i].continuation_line_
	  && (consecutive_lines.empty ()
	      || (consecutive_lines[0]->get_bound (LEFT)->get_column ()
	          == groups_[i].continuation_line_->get_bound (LEFT)->get_column ()
		  && consecutive_lines[0]->get_bound (RIGHT)->get_column ()
	          == groups_[i].continuation_line_->get_bound (RIGHT)->get_column ())))
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

  if (alignment_)
    {
      announce_end_grob (alignment_, SCM_EOL);
      alignment_ = 0;
    }

  if (to_boolean (get_property ("figuredBassCenterContinuations")))
    center_repeated_continuations ();
  
  for (vsize i = 0; i < groups_.size (); i++)
    {
      if (groups_[i].group_)
	{
	  announce_end_grob (groups_[i].group_ , SCM_EOL);
	  groups_[i].group_ = 0;
	}
      
      if (groups_[i].continuation_line_)
	{
	  announce_end_grob (groups_[i].continuation_line_ , SCM_EOL);
	  groups_[i].continuation_line_ = 0;
	}
    }

  /* Check me, groups_.clear () ? */
}

void
Figured_bass_engraver::add_brackets ()
{
  vector<Grob*> encompass;
  bool inside = false;
  for (vsize i = 0; i < groups_.size (); i ++)
    {
      if (!groups_[i].current_event_)
	continue;
      
      if (to_boolean (groups_[i].current_event_->get_property ("bracket-start")))	
	inside = true;

      if (inside && groups_[i].figure_item_)
	encompass.push_back (groups_[i].figure_item_);

       if (to_boolean (groups_[i].current_event_->get_property ("bracket-stop")))
	{
	  inside = false;

	  Item * brack = make_item ("BassFigureBracket", groups_[i].current_event_->self_scm ());
	  for (vsize j = 0; j < encompass.size (); j++)
	    {
	      Pointer_group_interface::add_grob (brack,
						 ly_symbol2scm ("elements"),
						 encompass[j]);
	    }
	  encompass.clear ();
	}
    }
}

void
Figured_bass_engraver::process_music ()
{
  if (alignment_ && !to_boolean (get_property ("useBassFigureExtenders")))
    clear_spanners ();
        
  if (rest_event_)
    {
      clear_spanners ();
      groups_.clear ();
      return;
    }
  
  if (!continuation_
      && new_events_.empty ())
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
  bool use_extenders = to_boolean (get_property ("useBassFigureExtenders"));
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
      while (k < groups_.size ()
	     && groups_[k].current_event_)
	k++;
      
      if (k >= groups_.size ())
	{
	  Figure_group group;
	  groups_.push_back (group);
	}
      
      groups_[k].current_event_ = new_events_[i];
      groups_[k].figure_item_ = 0;
      k++;
    }

  for (vsize i = 0; i < groups_.size (); i++)
    {
      if (!groups_[i].is_continuation ())
	{
	  groups_[i].number_ = SCM_BOOL_F;
	  groups_[i].alteration_ = SCM_BOOL_F;
	  groups_[i].augmented_ = SCM_BOOL_F;
	  groups_[i].diminished_ = SCM_BOOL_F;
	  groups_[i].augmented_slash_ = SCM_BOOL_F;
	  groups_[i].text_ = SCM_BOOL_F;
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
		  Spanner * line
		    = make_spanner ("BassFigureContinuation", SCM_EOL);
		  Item * item = group.figure_item_;
		  group.continuation_line_ = line;
		  line->set_bound (LEFT, item);

		  /*
		    Don't add as child. This will cache the wrong
		    (pre-break) stencil when callbacks are triggered.
		  */
		  line->set_parent (group.group_, Y_AXIS);
		  Pointer_group_interface::add_grob (line, ly_symbol2scm ("figures"), item);

		  group.figure_item_ = 0;
		}
	    }
	  else if (group.continuation_line_) 
	    junk_continuations.push_back (i); 
	}

      /*
	Ugh, repeated code.
       */
      vector<Spanner*> consecutive;
      if (to_boolean (get_property ("figuredBassCenterContinuations")))
	{
	  for (vsize i = 0; i <= junk_continuations.size (); i++)
	    {
	      if (i < junk_continuations.size ()
		  && (i == 0 || junk_continuations[i-1] == junk_continuations[i] - 1))
		consecutive.push_back (groups_[junk_continuations[i]].continuation_line_);
	      else 
		{
		  center_continuations (consecutive);
		  consecutive.clear ();
		  if (i < junk_continuations.size ())
		    consecutive.push_back (groups_[junk_continuations[i]].continuation_line_);
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
  Grob *muscol
    = dynamic_cast<Item*> (unsmob_grob (get_property ("currentMusicalColumn")));
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
	    = make_item ("BassFigure",
			 group.current_event_->self_scm ());

	  
	  SCM fig = group.current_event_->get_property ("figure");
	  if (!group.group_)
	    {
	      group.group_ = make_spanner ("BassFigureLine", SCM_EOL);
	      group.group_->set_bound (LEFT, muscol);
	      Align_interface::add_element (alignment_,
					    group.group_);
	    }

	  if (scm_memq (fig, get_property ("implicitBassFigures")) != SCM_BOOL_F)
	    {
	      item->set_property ("transparent", SCM_BOOL_T); 
	      item->set_property ("implicit", SCM_BOOL_T);
	    }
	  
      	  group.number_ = fig;
      	  group.alteration_ = group.current_event_->get_property ("alteration");
	  group.augmented_ = group.current_event_->get_property ("augmented");
	  group.diminished_ = group.current_event_->get_property ("diminished");
	  group.augmented_slash_ = group.current_event_->get_property ("augmented-slash");
	  group.text_ = group.current_event_->get_property ("text");

	  SCM text = group.text_;
	  if (!Text_interface::is_markup (text)
	      && ly_is_procedure (proc))
	    {
	      text = scm_call_3 (proc, fig, group.current_event_->self_scm (),
				 context ()->self_scm ());
	    }

	  item->set_property ("text", text);
	  
	  Axis_group_interface::add_element (group.group_, item);
	  group.figure_item_ = item;
	}

      if (group.continuation_line_)
	{
	  /*
	    UGH should connect to the bass staff, and get the note heads. 
	  */
	  group.figure_item_->set_property ("transparent", SCM_BOOL_T);
	  group.continuation_line_->set_bound (RIGHT, group.figure_item_);
	}
      
      if (groups_[i].group_)
	groups_[i].group_->set_bound (RIGHT, muscol);

    }

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
		""
		);
