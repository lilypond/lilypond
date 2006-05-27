/*
  figured-bass-engraver.cc -- implement Figured_bass_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "engraver.hh"

#include "context.hh"
#include "music.hh"
#include "item.hh"
#include "spanner.hh"
#include "axis-group-interface.hh"
#include "align-interface.hh"
#include "pointer-group-interface.hh"
#include "text-interface.hh"
#include "grob-array.hh"


#include "translator.icc"

struct Figure_group
{
  Spanner *group_;
  Spanner *continuation_line_;
  
  SCM number_;
  SCM alteration_;
  
  Item *figure_item_; 
  Music *current_music_;
  bool force_no_continuation_;
  
  Figure_group ()
  {
    figure_item_ = 0;
    force_no_continuation_ = false;
    continuation_line_ = 0;
    number_ = SCM_EOL;
    alteration_ = SCM_EOL;
    group_ = 0;
    current_music_ = 0;
  }
  bool is_continuation () const
  {
    return
      current_music_
      && !force_no_continuation_
      && ly_is_equal (number_,
		      current_music_->get_property ("figure"))
      && ly_is_equal (alteration_,
		      current_music_->get_property ("alteration"));
  }
};

struct Figured_bass_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS(Figured_bass_engraver);
  void clear_spanners();
  void add_brackets ();
  void create_grobs ();

  void center_continuations (vector<Spanner*> const &consecutive_lines);
  void center_repeated_continuations ();
protected:
  vector<Figure_group> groups_;
  Spanner *alignment_;
  vector<Music*> new_musics_;
  bool continuation_;
  bool new_music_found_;
  
  Moment stop_moment_;
  Music *rest_event_; 
  
  virtual bool try_music (Music *);
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
    found  = found  || groups_[i].current_music_;

  if (!found)
    clear_spanners ();
}

Figured_bass_engraver::Figured_bass_engraver ()
{
  alignment_ = 0;
  continuation_ = false;
  rest_event_ = 0;
  new_music_found_ = false;
}

void
Figured_bass_engraver::start_translation_timestep ()
{
  if (now_mom ().main_part_ < stop_moment_.main_part_
      || now_mom ().grace_part_ < Rational (0))
    return ;
  
  rest_event_ = 0;
  new_musics_.clear ();
  for (vsize i = 0; i < groups_.size (); i++)
    groups_[i].current_music_ = 0;
  continuation_ = false;
}

bool
Figured_bass_engraver::try_music (Music *m)
{
  new_music_found_ = true;
  if (m->is_mus_type ("rest-event"))
    {
      rest_event_ = m;
      return true;
    }
  else
    {
      stop_moment_ = now_mom () + m->get_length ();
     
      SCM fig = m->get_property ("figure");
      for (vsize i = 0; i < groups_.size (); i++)
	{
	  if (!groups_[i].current_music_
	      && ly_is_equal (groups_[i].number_, fig))
	    {
	      groups_[i].current_music_ = m;
	      groups_[i].force_no_continuation_
		= to_boolean (m->get_property ("no-continuation"));
	      continuation_ = true;
	      return true; 
	    }
	}

      new_musics_.push_back (m);

      return true;
    }
}

void
Figured_bass_engraver::center_continuations (vector<Spanner*> const &consecutive_lines)
{
  if (consecutive_lines.size () == 2)
    {
      vector<Grob*> left_figs;
      for (vsize j = consecutive_lines.size(); j--;)
	left_figs.push_back (consecutive_lines[j]->get_bound (LEFT));

      SCM  ga = Grob_array::make_array ();
      unsmob_grob_array (ga)->set_array (left_figs);

      for (vsize j = consecutive_lines.size(); j--;)
	consecutive_lines[j]->set_object ("figures",
					  unsmob_grob_array (ga)->smobbed_copy ());
    }
}

void
Figured_bass_engraver::center_repeated_continuations ()
{  
  vector<Spanner*> consecutive_lines;
  for (vsize i = 0; i <= groups_.size(); i++)
    {
      if (i < groups_.size ()
	  && groups_[i].continuation_line_
	  && (consecutive_lines.empty ()
	      || (consecutive_lines[0]->get_bound(LEFT)->get_column ()
	          == groups_[i].continuation_line_->get_bound (LEFT)->get_column ()
		  && consecutive_lines[0]->get_bound(RIGHT)->get_column ()
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
  
  alignment_ = 0;
  if (to_boolean (get_property ("figuredBassCenterContinuations")))
    center_repeated_continuations();
  
  groups_.clear ();
}

void
Figured_bass_engraver::add_brackets ()
{
  vector<Grob*> encompass;
  bool inside = false;
  for (vsize i = 0; i < groups_.size (); i ++)
    {
      if (!groups_[i].current_music_)
	continue;
      
      if (to_boolean (groups_[i].current_music_->get_property ("bracket-start")))	
	inside = true;

      if (inside && groups_[i].figure_item_)
	encompass.push_back (groups_[i].figure_item_);

       if (to_boolean (groups_[i].current_music_->get_property ("bracket-stop")))
	{
	  inside = false;

	  Item * brack = make_item ("BassFigureBracket", groups_[i].current_music_->self_scm ());
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
  if (rest_event_)
    {
      clear_spanners ();
      return;
    }
  
  if (!continuation_
      && new_musics_.empty ())
    {
      clear_spanners ();
      return;
    }

  if (!new_music_found_)
    return;
  
  new_music_found_ = false;

  /*
    Don't need to sync alignments, if we're not using extenders. 
   */
  bool use_extenders = to_boolean (get_property ("useBassFigureExtenders"));
  if (!use_extenders)
    {
      if (to_boolean (get_property ("figuredBassCenterContinuations")))
	center_repeated_continuations ();
      
      alignment_ = 0;
      for (vsize i = 0; i < groups_.size (); i++)
	{
	  groups_[i].group_ = 0;
	  groups_[i].continuation_line_ = 0;
	}
    }
  
  if (!continuation_)
    clear_spanners ();
  
  vsize k = 0;
  for (vsize i = 0; i < new_musics_.size (); i++)
    {
      while (k < groups_.size ()
	     && groups_[k].current_music_)
	k++;
      
      if (k >= groups_.size ())
	{
	  Figure_group group;
	  groups_.push_back (group);
	}
      
      groups_[k].current_music_ = new_musics_[i];
      groups_[k].figure_item_ = 0;
      k++;
    }

  for (vsize i = 0; i < groups_.size (); i++)
    {
      if (!groups_[i].is_continuation ())
	{
	  groups_[i].number_ = SCM_BOOL_F;
	  groups_[i].alteration_ = SCM_BOOL_F;
	}
    }

  if (use_extenders)
    {
      vector<int> junk_continuations;
      for (vsize i = 0; i < groups_.size(); i++)
	{
	        Figure_group &group = groups_[i];

	  if (group.is_continuation ())
	    {
	      if (!group.continuation_line_)
		{
		  Spanner * line = make_spanner ("BassFigureContinuation", SCM_EOL);
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
	      if (i < junk_continuations.size()
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
  Grob *muscol = dynamic_cast<Item*> (unsmob_grob (get_property ("currentMusicalColumn")));
  if (!alignment_)
    {
      alignment_ = make_spanner ("BassFigureAlignment", SCM_EOL);
      alignment_->set_bound (LEFT, muscol);
    }
  alignment_->set_bound (RIGHT, muscol);

  SCM proc = get_property ("figuredBassFormatter");
  for (vsize i = 0; i < groups_.size(); i++)
    {
      Figure_group &group = groups_[i];
      
      if (group.current_music_)
	{
	  Item *item
	    = make_item ("BassFigure",
			 group.current_music_->self_scm ());

	  
	  SCM fig = group.current_music_->get_property ("figure");
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
      	  group.alteration_ = group.current_music_->get_property ("alteration");

	  SCM text = group.current_music_->get_property ("text");
	  if (!Text_interface::is_markup (text)
	      && ly_is_procedure (proc))
	    {
	      text = scm_call_3 (proc, fig, group.current_music_->self_scm (),
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
		"BassFigureLine "
		,
		/* accept */
		"bass-figure-event rest-event",

		/* read */
		"figuredBassAlterationDirection "
		"figuredBassCenterContinuations "
		"figuredBassFormatter "
		"implicitBassFigures "
		"useBassFigureExtenders "
		,

		/* write */
		"");
