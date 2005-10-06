/*
  new-figured-bass-engraver.cc -- implement New_figured_bass_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "translator.icc"

struct Figure_group
{
  Spanner *group_;
  Spanner *continuation_line_;
  
  SCM number_;
  SCM alteration_;
  
  bool is_continuation_;
  Item *figure_item_; 
  Music *current_music_;
  
  Figure_group ()
  {
    is_continuation_ = false;
    continuation_line_ = 0;
    number_ = SCM_EOL;
    alteration_ = SCM_EOL;
    group_ = 0;
    current_music_ = 0;
  }
};

struct New_figured_bass_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS(New_figured_bass_engraver);
  void clear_spanners();
  void add_brackets ();
protected:
  Array<Figure_group> groups_;
  Spanner *alignment_;
  Link_array<Music> new_musics_;
  bool continuation_;
  Moment stop_moment_;
  Music *rest_event_; 
  
  virtual bool try_music (Music *);
  virtual void derived_mark () const; 

  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();
};

void
New_figured_bass_engraver::derived_mark () const
{
  for (int i = 0; i < groups_.size (); i++)
    {
      scm_gc_mark (groups_[i].number_);
      scm_gc_mark (groups_[i].alteration_);
    }
}

void
New_figured_bass_engraver::stop_translation_timestep ()
{
  if (groups_.is_empty ()
      || now_mom ().main_part_ < stop_moment_.main_part_)
    return ;
  
  bool found = false;
  for (int i = 0; !found && i < groups_.size (); i++)
    found  = found  || groups_[i].current_music_;

  if (!found)
    clear_spanners ();
}

New_figured_bass_engraver::New_figured_bass_engraver ()
{
  alignment_ = 0;
  continuation_ = false;
  rest_event_ = 0;
}

void
New_figured_bass_engraver::start_translation_timestep ()
{
  if (now_mom ().main_part_ < stop_moment_.main_part_)
    return ;
  
  rest_event_ = 0;
  new_musics_.clear ();
  for (int i = 0; i < groups_.size (); i++)
    {
      groups_[i].current_music_ = 0;
      groups_[i].is_continuation_ = false;
    }
  continuation_ = false;
}

bool
New_figured_bass_engraver::try_music (Music *m)
{
 if (m->is_mus_type ("rest-event"))
    {
      rest_event_ = m;
      return true;
    }
 else
   {
     SCM fig = m->get_property ("figure");
     for (int i = 0; i < groups_.size (); i++)
       {
	 if (!groups_[i].current_music_
	     && ly_is_equal (groups_[i].number_, fig))
	   {
	     groups_[i].current_music_ = m;
	     groups_[i].is_continuation_ =
	       ly_is_equal (groups_[i].alteration_,
			    m->get_property ("alteration"));
	     
	     continuation_ = true;
	     return true; 
	   }
       }

     new_musics_.push (m);

     stop_moment_ = now_mom () + m->get_length ();
     
     return true;
   }
}

void
New_figured_bass_engraver::clear_spanners ()
{
  if (!alignment_)
    return;
  
  alignment_ = 0;
  groups_.clear ();
}

void
New_figured_bass_engraver::add_brackets ()
{
  Link_array<Grob> encompass;
  bool inside = false;
  for (int i = 0; i < groups_.size (); i ++)
    {
      if (!groups_[i].current_music_)
	continue;
      
      if (to_boolean (groups_[i].current_music_->get_property ("bracket-start")))	
	{
	  inside = true;
	}

      if (inside && groups_[i].figure_item_)
	encompass.push (groups_[i].figure_item_);

       if (to_boolean (groups_[i].current_music_->get_property ("bracket-stop")))
	{
	  inside = false;

	  Item * brack = make_item ("BassFigureBracket", groups_[i].current_music_->self_scm ());
	  for (int j = 0; j < encompass.size (); j++)
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
New_figured_bass_engraver::process_music ()
{
  if (rest_event_)
    {
      clear_spanners ();
      return;
    }
  
  if (!continuation_
      && new_musics_.is_empty ())
    {
      clear_spanners ();
      return;
    }
  
  Grob *muscol = dynamic_cast<Item*> (unsmob_grob (get_property ("currentMusicalColumn")));
  if (!continuation_)
    {
      clear_spanners ();
      alignment_ = make_spanner ("BassFigureAlignment", SCM_EOL);
      alignment_->set_bound (LEFT, muscol);
    }

  int k = 0;
  for (int i = 0; i < new_musics_.size (); i++)
    {
      while (k < groups_.size() &&
	     groups_[k].current_music_)
	k++;
      
      if (k >= groups_.size ())
	{
	  Figure_group group;
	  groups_.push (group);
	}
      
      groups_[k].current_music_ = new_musics_[i];
      groups_[k].figure_item_ = 0;
      k++;
    }

  for (int i = 0; i < groups_.size (); i++)
    {
      if (!groups_[i].is_continuation_)
	{
	  groups_[i].number_ = SCM_BOOL_F;
	  groups_[i].alteration_ = SCM_BOOL_F;
	}
    }

  SCM proc = get_property ("newFiguredBassFormatter");
  alignment_->set_bound (RIGHT, muscol);

  if (to_boolean (get_property ("useBassFigureExtenders")))
    for (int i = 0; i < groups_.size(); i++)
      {
	if (groups_[i].is_continuation_)
	  {
	    if (!groups_[i].continuation_line_)
	      {
		Spanner * line = make_spanner ("BassFigureContinuation", SCM_EOL);
		Item * item = groups_[i].figure_item_;
		groups_[i].continuation_line_ = line;
		line->set_bound (LEFT, item);

		/*
		  Don't add as child. This will cache the wrong
		  (pre-break) stencil when callbacks are triggered.
		*/
		line->set_parent (groups_[i].group_, Y_AXIS);
		Pointer_group_interface::add_grob (line, ly_symbol2scm ("figures"), item);
	      
		groups_[i].figure_item_ = 0;
	      }
	  }
	else
	  groups_[i].continuation_line_ = 0;
      }
  
  for (int i = 0; i < groups_.size(); i++)
    {
      Figure_group &group = groups_[i];
      
      if (group.continuation_line_)
	{
	  group.continuation_line_->set_bound (RIGHT, muscol);
	}
      else if (group.current_music_)
	{
	  Item *item
	    = make_item ("NewBassFigure",
			 group.current_music_->self_scm ());

	  SCM fig = group.current_music_->get_property ("figure");
	  if (!group.group_)
	    {
	      group.group_ = make_spanner ("BassFigureLine", SCM_EOL);
	      group.group_->set_bound (LEFT, muscol);
	      Align_interface::add_element (alignment_,
					    group.group_,
					    Align_interface::alignment_callback_proc);
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

      groups_[i].group_->set_bound (RIGHT, muscol);
    }

  add_brackets ();
}


ADD_TRANSLATOR (New_figured_bass_engraver,
		/* doc */

		"Make figured bass numbers.",
		/* create */
		"NewBassFigure "
		"BassFigureAlignment "
		"BassFigureBracket",
		"BassFigureContinuation "
		"BassFigureLine "

		/* accept */
		"bass-figure-event rest-event",

		/* read */
		"useBassFigureExtenders",

		/* write */
		"");
